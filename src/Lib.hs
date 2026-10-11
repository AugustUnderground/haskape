{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib ( SignalType(..), Edge(..), Pin(..)
           , NX, NY, NL, NS
           , Layer, Slot, Index, Offset
           , BGA(..), CandidateElt, Fanout
           , natInt, numLayers, gridWidth, gridHeight, numSignalTypes
           , encodePin, decodePin, layerAllowed, edgeAllowed, anchorSlot, viaAnchor'
             -- BGA / fanout construction and validation
           , mkBGA, validatePinout, randomPinout, showBGA
           , validateFanout, randomFanout, randomFanoutWithGen
             -- Loss and annealing
           , fanoutOccupancyCost, fanoutPreferenceCostAcc, fanoutLoss
           , AnnealConfig(..), AnnealResult(..)
           , acceptMove, coolingTemperature, proposeFanoutMove, annealStep
           , simulatedAnnealing, runAnnealingChunks
           ) where

import           Control.Monad (forM_)
import           Data.Maybe (isJust)
import qualified Data.Vector as V
import           Data.Proxy (Proxy(..))
import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.TypeNats (Nat, KnownNat, natVal)
import           System.Random (StdGen, newStdGen, randomRs, randomR)
import           Text.Printf (printf)

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU

--------------------------------------------------------------------------------
-- Domain model and dimensions
--------------------------------------------------------------------------------

data SignalType = A | B | C | D
  deriving (Eq, Ord, Show, Enum, Bounded)

data Edge = North | East | South | West
  deriving (Eq, Ord, Show, Enum, Bounded)

data Pin = Ground | Signal !SignalType
  deriving (Eq, Show)

type NX = 200
type NY = 100
type NL = 5
type NS = 12

type Layer  = Int
type Slot   = Int
type Index  = Int
type Offset = Int

type CandidateElt = ( Int  -- anchor X
                    , Int  -- anchor Y
                    , Int  -- X slot pick
                    , Int  -- Y slot pick
                    , Int  -- edge code
                    , Int  -- layer; -1 denotes a ground via
                    )

type Fanout = A.Array A.DIM2 CandidateElt

newtype BGA (nx :: Nat) (ny :: Nat) (nl :: Nat) = BGA (A.Array A.DIM3 Int)

natInt :: forall n. KnownNat n => Int
natInt = fromIntegral (natVal (Proxy @n))

numLayers :: Int
numLayers = natInt @NL

gridWidth :: Int
gridWidth = natInt @NX * natInt @NS

gridHeight :: Int
gridHeight = natInt @NY * natInt @NS

numSignalTypes :: Int
numSignalTypes = fromEnum (maxBound :: SignalType) - fromEnum (minBound :: SignalType) + 1

--------------------------------------------------------------------------------
-- Pin encoding, slot selection, and routing policies
--------------------------------------------------------------------------------

slotOffset :: Slot -> Maybe Offset
slotOffset choice
  | choice < 0  = Nothing
  | choice == 0 = Just 0
  | odd choice  = Just ((choice + 1) `div` 2)
  | otherwise   = Just (negate (choice `div` 2))

anchorSlot :: Int -> Index -> Slot
anchorSlot ns coordinate
  | ns <= 0   = error "NS must be positive"
  | otherwise = coordinate * ns + (ns - 1) `div` 2

viaAnchor' :: Index -> Index -> (Int, Int)
viaAnchor' x y = (anchorSlot (natInt @NS) x, anchorSlot (natInt @NS) y)

encodePin :: Pin -> Int
encodePin Ground     = 0
encodePin (Signal s) = fromEnum s - fromEnum (minBound :: SignalType) + 1

decodePin :: Int -> Maybe Pin
decodePin 0 = Just Ground
decodePin n
  | n >= 1 && n <= numSignalTypes =
      Just $ Signal $ toEnum (fromEnum (minBound :: SignalType) + n - 1)
  | otherwise = Nothing

-- Rows are indexed by signal type, then by layer or edge code.
layerPolicyRows :: [[Int]] -- M1 M2 M3 M4 M5
layerPolicyRows =
  [ [0, 1, 1, 1, 0] -- A
  , [1, 1, 0, 0, 0] -- B
  , [0, 0, 0, 0, 1] -- C
  , [0, 0, 0, 0, 1] -- D
  ]

layerAllowed :: SignalType -> Layer -> Bool
layerAllowed signal layer'
  | layer' < 0 || layer' >= numLayers = False
  | otherwise = layerPolicyRows !! fromEnum signal !! layer' == 1

edgePolicyRows :: [[Int]] -- North East South West
edgePolicyRows =
  [ [1, 0, 1, 0] -- A: North / South
  , [1, 0, 1, 0] -- B: North / South
  , [0, 1, 0, 1] -- C: East / West
  , [0, 1, 0, 1] -- D: East / West
  ]

edgeAllowed :: SignalType -> Edge -> Bool
edgeAllowed signal edge' = edgePolicyRows !! fromEnum signal !! fromEnum edge' == 1

maxSlotChoices :: Int
maxSlotChoices = 3 * natInt @NS

--------------------------------------------------------------------------------
-- Candidate geometry and validation
--------------------------------------------------------------------------------

type Point2 = (Int, Int)

data CandidateGeometry
  = GroundVia !Point2
  | SignalTrace !Point2 !Layer !Edge ![Point2]
  deriving (Eq, Show)

candidateAnchor :: CandidateGeometry -> Point2
candidateAnchor (GroundVia p)         = p
candidateAnchor (SignalTrace p _ _ _) = p

viaFloor :: CandidateGeometry -> Layer
viaFloor (GroundVia _)         = 0
viaFloor (SignalTrace _ l _ _) = l

strictlyBetween :: Int -> Int -> Int -> Bool
strictlyBetween x a b = min a b < x && x < max a b

validAnchorCoordinate :: Int -> Int -> Bool
validAnchorCoordinate ballCount coordinate
  | ns <= 0 = False
  | otherwise = coordinate >= firstAnchor
            && coordinate < ballCount * ns
            && (coordinate - firstAnchor) `mod` ns == 0
  where
    ns = natInt @NS
    firstAnchor = (ns - 1) `div` 2

resolvePick :: String -> Int -> Int -> Slot -> Either String Int
resolvePick label extent anchor pick
  | maxSlotChoices <= 0 = Left "maxSlotChoices must be positive"
  | pick < 0 || pick >= maxSlotChoices =
      Left $ label ++ " pick outside search limit: " ++ show pick
  | otherwise = case slotOffset pick of
      Nothing -> Left $ "Invalid " ++ label ++ " pick: " ++ show pick
      Just offset ->
        let target = anchor + offset
        in if target >= 0 && target < extent
             then Right target
             else Left $ label ++ " target outside grid: " ++ show target

candidatePoints :: Edge -> Int -> Int -> Int -> Int -> Either String [Point2]
candidatePoints North ax ay xp yp
  | xp == 0 = Right [(ax, ay), (ax, 0)]
  | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      jy <- resolvePick "Y" gridHeight ay yp
      if strictlyBetween jy ay 0
        then Right [(ax, ay), (ax, jy), (tx, jy), (tx, 0)]
        else Left "North jog must lie strictly between source and edge"
candidatePoints South ax ay xp yp
  | xp == 0 = Right [(ax, ay), (ax, gridHeight - 1)]
  | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      jy <- resolvePick "Y" gridHeight ay yp
      let edgeY = gridHeight - 1
      if strictlyBetween jy ay edgeY
        then Right [(ax, ay), (ax, jy), (tx, jy), (tx, edgeY)]
        else Left "South jog must lie strictly between source and edge"
candidatePoints East ax ay xp yp
  | yp == 0 = Right [(ax, ay), (gridWidth - 1, ay)]
  | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      ty <- resolvePick "Y" gridHeight ay yp
      let edgeX = gridWidth - 1
      if strictlyBetween tx ax edgeX
        then Right [(ax, ay), (tx, ay), (tx, ty), (edgeX, ty)]
        else Left "East jog must lie strictly between source and edge"
candidatePoints West ax ay xp yp
  | yp == 0 = Right [(ax, ay), (0, ay)]
  | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      ty <- resolvePick "Y" gridHeight ay yp
      if strictlyBetween tx ax 0
        then Right [(ax, ay), (tx, ay), (tx, ty), (0, ty)]
        else Left "West jog must lie strictly between source and edge"

candidateGeometry :: CandidateElt -> Either String CandidateGeometry
candidateGeometry (ax, ay, xp, yp, e, l)
  | not (validAnchorCoordinate (natInt @NX) ax) =
      Left $ "Invalid anchor X: " ++ show ax
  | not (validAnchorCoordinate (natInt @NY) ay) =
      Left $ "Invalid anchor Y: " ++ show ay
  | l == -1 =
      if (xp, yp, e) == (-1, -1, -1)
        then Right $ GroundVia (ax, ay)
        else Left "Malformed ground candidate"
  | l < 0 || l >= numLayers = Left $ "Invalid layer: " ++ show l
  | otherwise = do
      edge' <- decodeEdgeCode e
      points <- candidatePoints edge' ax ay xp yp
      Right $ SignalTrace (ax, ay) l edge' points
  where
    decodeEdgeCode 0 = Right North
    decodeEdgeCode 1 = Right East
    decodeEdgeCode 2 = Right South
    decodeEdgeCode 3 = Right West
    decodeEdgeCode n = Left $ "Invalid edge code: " ++ show n

pathSegments :: [Point2] -> [(Point2, Point2)]
pathSegments points = zip points (drop 1 points)

betweenInclusive :: Int -> Int -> Int -> Bool
betweenInclusive a b p = min a b <= p && p <= max a b

intervalsOverlap :: Int -> Int -> Int -> Int -> Bool
intervalsOverlap a b c d = max (min a b) (min c d) <= min (max a b) (max c d)

segmentsIntersect :: (Point2, Point2) -> (Point2, Point2) -> Bool
segmentsIntersect ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | y1 == y2 && y3 == y4 = y1 == y3 && intervalsOverlap x1 x2 x3 x4
  | x1 == x2 && x3 == x4 = x1 == x3 && intervalsOverlap y1 y2 y3 y4
  | y1 == y2 && x3 == x4 = betweenInclusive x1 x2 x3 && betweenInclusive y3 y4 y1
  | x1 == x2 && y3 == y4 = betweenInclusive x3 x4 x1 && betweenInclusive y1 y2 y3
  | otherwise = False

pathsIntersect :: [Point2] -> [Point2] -> Bool
pathsIntersect a b = or
  [ segmentsIntersect sa sb
  | sa <- pathSegments a
  , sb <- pathSegments b
  ]

pointOnPath :: Point2 -> [Point2] -> Bool
pointOnPath (x, y) points = any onSegment (pathSegments points)
  where
    onSegment ((x1, y1), (x2, y2))
      | y1 == y2 = y == y1 && betweenInclusive x1 x2 x
      | x1 == x2 = x == x1 && betweenInclusive y1 y2 y
      | otherwise = False

traceInfo :: CandidateGeometry -> Maybe (Layer, [Point2])
traceInfo (GroundVia _)              = Nothing
traceInfo (SignalTrace _ l _ points) = Just (l, points)

traceHitsVia :: CandidateGeometry -> CandidateGeometry -> Bool
traceHitsVia route via = case traceInfo route of
  Just (routeLayer, points) ->
    routeLayer >= viaFloor via && pointOnPath (candidateAnchor via) points
  Nothing -> False

candidateGeometriesConflict :: CandidateGeometry -> CandidateGeometry -> Bool
candidateGeometriesConflict a b =
     candidateAnchor a == candidateAnchor b
  || sameLayerTraceConflict
  || traceHitsVia a b
  || traceHitsVia b a
  where
    sameLayerTraceConflict = case (traceInfo a, traceInfo b) of
      (Just (la, pa), Just (lb, pb)) -> la == lb && pathsIntersect pa pb
      _ -> False

validateCandidateForPin :: Int -> Int -> Int -> CandidateElt -> Either String CandidateGeometry
validateCandidateForPin bx by pinCode candidate = do
  pin <- maybe (Left $ "Invalid pin code: " ++ show pinCode) Right (decodePin pinCode)
  geometry <- candidateGeometry candidate
  if candidateAnchor geometry /= expectedAnchor
    then Left $ "Candidate anchor does not match BGA coordinate " ++ show (bx, by)
    else pure ()
  case (pin, geometry) of
    (Ground, GroundVia _) -> Right geometry
    (Ground, _) -> Left "Ground pin must use the ground candidate sentinel"
    (Signal _, GroundVia _) -> Left "Signal pin cannot use the ground candidate sentinel"
    (Signal signal, SignalTrace _ l edge' _) -> do
      if layerAllowed signal l
        then pure ()
        else Left $ "Layer " ++ show l ++ " is forbidden for " ++ show signal
      if edgeAllowed signal edge'
        then pure ()
        else Left $ "Edge " ++ show edge' ++ " is forbidden for " ++ show signal
      Right geometry
  where
    expectedAnchor = viaAnchor' bx by

pairwiseConflicts :: [((Int, Int), CandidateGeometry)] -> [String]
pairwiseConflicts [] = []
pairwiseConflicts ((coordA, a) : rest) =
  [ "Route conflict between BGA " ++ show coordA ++ " and BGA " ++ show coordB
  | (coordB, b) <- rest
  , candidateGeometriesConflict a b
  ] ++ pairwiseConflicts rest

validateFanout :: forall nx ny. (KnownNat nx, KnownNat ny)
               => BGA nx ny 1 -> Fanout -> Either [String] ()
validateFanout (BGA pinArr) fanout
  | not pinShapeCorrect = Left ["BGA array dimensions do not match its type"]
  | not fanoutShapeCorrect = Left ["Fanout array dimensions do not match BGA dimensions"]
  | length pinCodes /= nx * ny = Left ["BGA has an incorrect number of pin codes"]
  | length candidates /= nx * ny = Left ["Fanout has an incorrect number of candidates"]
  | not (null localErrors) = Left localErrors
  | not (null conflicts) = Left conflicts
  | otherwise = Right ()
  where
    nx = natInt @nx
    ny = natInt @ny
    pinShapeCorrect = case A.arrayShape pinArr of
      A.Z A.:. px A.:. py A.:. pl -> px == nx && py == ny && pl == 1
    fanoutShapeCorrect = case A.arrayShape fanout of
      A.Z A.:. fx A.:. fy -> fx == nx && fy == ny
    pinCodes = A.toList pinArr
    candidates = A.toList fanout
    coords = [(x, y) | x <- [0 .. nx - 1], y <- [0 .. ny - 1]]
    checked =
      [ ((x, y), validateCandidateForPin x y pinCode candidate)
      | ((x, y), (pinCode, candidate)) <- zip coords (zip pinCodes candidates)
      ]
    localErrors =
      [ "BGA " ++ show coord ++ ": " ++ err
      | (coord, Left err) <- checked
      ]
    validCandidates =
      [ (coord, geometry)
      | (coord, Right geometry) <- checked
      ]
    conflicts = pairwiseConflicts validCandidates

--------------------------------------------------------------------------------
-- Random pinout and candidate generation
--------------------------------------------------------------------------------

chooseOne :: [a] -> StdGen -> Either String (a, StdGen)
chooseOne [] _ = Left "Cannot choose from an empty list"
chooseOne xs gen = Right (xs !! i, gen')
  where
    (i, gen') = randomR (0, length xs - 1) gen

pickCoordinate :: Int -> Int -> Slot -> Maybe Int
pickCoordinate extent anchor pick = do
  offset <- slotOffset pick
  let target = anchor + offset
  if target >= 0 && target < extent then Just target else Nothing

randomCandidate :: Int -> Int -> Int -> Int -> Int -> StdGen
                -> Either String (CandidateElt, StdGen)
randomCandidate nx ny bx by pinCode gen0
  | pinCode == 0 = Right ((ax, ay, -1, -1, -1, -1), gen0)
  | pinCode < 1 || pinCode > numSignalTypes =
      Left $ "Invalid pin code: " ++ show pinCode
  | null allowedLayers = Left "Signal type has no eligible metal layers"
  | null allowedEdges = Left "Signal type has no eligible edges"
  | otherwise = do
      (edge', g1) <- chooseOne allowedEdges gen0
      (layer', g2) <- chooseOne allowedLayers g1
      let (kind', g3) = randomR (0 :: Int, 1 :: Int) g2
      if kind' == 0
        then makeDirect edge' layer' g3
        else makeJog edge' layer' g3
  where
    ns = natInt @NS
    gridX = nx * ns
    gridY = ny * ns
    ax = anchorSlot ns bx
    ay = anchorSlot ns by
    signal = toEnum (pinCode - 1) :: SignalType
    allowedLayers = [l | l <- [0 .. numLayers - 1], layerAllowed signal l]
    allowedEdges = [e | e <- [North, East, South, West], edgeAllowed signal e]
    makeDirect edge'' layer'' gen =
      Right ((ax, ay, 0, 0, fromEnum edge'', layer''), gen)
    makeJog edge'' layer'' gen =
      let isNS = edge'' == North || edge'' == South
          targetExtent = if isNS then gridX else gridY
          targetAnchor = if isNS then ax else ay
          approachExtent = if isNS then gridY else gridX
          approachAnchor = if isNS then ay else ax
          boundary = case edge'' of
            North -> 0
            East  -> gridX - 1
            South -> gridY - 1
            West  -> 0
          targetPicks =
            [ p
            | p <- [1 .. maxSlotChoices - 1]
            , isJust (pickCoordinate targetExtent targetAnchor p)
            ]
          approachPicks =
            [ p
            | p <- [1 .. maxSlotChoices - 1]
            , Just coord <- [pickCoordinate approachExtent approachAnchor p]
            , strictlyBetween coord approachAnchor boundary
            ]
      in if null targetPicks || null approachPicks
           then makeDirect edge'' layer'' gen
           else do
             (targetPick, g1) <- chooseOne targetPicks gen
             (approachPick, g2) <- chooseOne approachPicks g1
             let (xp, yp) = if isNS
                              then (targetPick, approachPick)
                              else (approachPick, targetPick)
             Right ((ax, ay, xp, yp, fromEnum edge'', layer''), g2)

randomFanoutWithGen :: forall nx ny. (KnownNat nx, KnownNat ny)
                    => BGA nx ny 1 -> StdGen -> Either String (Fanout, StdGen)
randomFanoutWithGen (BGA pinArr) gen0 = do
  if length pinCodes /= nx * ny
    then Left "Pinout has incorrect dimensions"
    else do
      (choices, gen1) <- go (zip coords pinCodes) gen0 []
      let fanout = A.fromList (A.Z A.:. nx A.:. ny) choices
      pure (fanout, gen1)
  where
    go [] gen acc = Right (reverse acc, gen)
    go (((x, y), pinCode) : rest) gen acc = do
      (choice, gen') <- randomCandidate (natInt @nx) (natInt @ny) x y pinCode gen
      go rest gen' (choice : acc)
    nx = natInt @nx
    ny = natInt @ny
    pinCodes = A.toList pinArr
    coords = [(x, y) | x <- [0 .. nx - 1], y <- [0 .. ny - 1]]

randomFanout :: forall nx ny. (KnownNat nx, KnownNat ny)
             => BGA nx ny 1 -> IO Fanout
randomFanout bga = do
  gen <- newStdGen
  case randomFanoutWithGen @nx @ny bga gen of
    Left err -> fail err
    Right (fanout, _) -> pure fanout

--------------------------------------------------------------------------------
-- Accelerate route segments, difference arrays, and occupancy
--------------------------------------------------------------------------------

type SegmentElt = ( Int  -- x1
                  , Int  -- y1
                  , Int  -- x2
                  , Int  -- y2
                  , Int  -- layer; -1 if inactive
                  , Int  -- active: 1=yes, 0=no
                  )

type SegmentEventElt = (Int, Int, Int, Int)

routeBallCount :: Int
routeBallCount = natInt @NX * natInt @NY

pickOffsetExp :: A.Exp Int -> A.Exp Int
pickOffsetExp pick =
  A.cond (pick A.== 0) 0 $
    A.cond (A.rem pick 2 A.== 1)
      (A.quot (pick A.+ 1) 2)
      (0 A.- A.quot pick 2)

routeSegmentsAcc :: A.Acc Fanout -> A.Acc (A.Vector SegmentElt)
routeSegmentsAcc fanout =
  A.generate (A.constant (A.Z A.:. (routeBallCount * 3))) $ \ix ->
    let outputIx = A.unindex1 ix
        routeIx = A.quot outputIx 3
        segmentIx = A.rem outputIx 3
        flatCandidates = A.reshape (A.index1 (A.constant routeBallCount)) fanout
        candidate = flatCandidates A.! A.index1 routeIx
        (ax, ay, xp, yp, edgeCode, layerCode) =
          A.unlift candidate
            :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)
        isNorth = edgeCode A.== 0
        isEast = edgeCode A.== 1
        isSouth = edgeCode A.== 2
        isNorthSouth = A.cond isNorth (A.constant True) isSouth
        isDirect = A.cond isNorthSouth (xp A.== 0) (yp A.== 0)
        segmentCount = A.cond isDirect 1 3
        targetX = ax A.+ pickOffsetExp xp
        targetY = ay A.+ pickOffsetExp yp
        terminalX = A.cond isNorthSouth targetX $
          A.cond isEast (A.constant (gridWidth - 1)) (A.constant 0)
        terminalY = A.cond isNorth (A.constant 0) $
          A.cond isSouth (A.constant (gridHeight - 1)) targetY
        jog1X = A.cond isNorthSouth ax targetX
        jog1Y = A.cond isNorthSouth targetY ay
        p1X = A.cond isDirect terminalX jog1X
        p1Y = A.cond isDirect terminalY jog1Y
        p2X = A.cond isDirect terminalX targetX
        p2Y = A.cond isDirect terminalY targetY
        pointX :: A.Exp Int -> A.Exp Int
        pointX i = A.cond (i A.== 0) ax $ A.cond (i A.== 1) p1X $
                     A.cond (i A.== 2) p2X terminalX
        pointY :: A.Exp Int -> A.Exp Int
        pointY i = A.cond (i A.== 0) ay $ A.cond (i A.== 1) p1Y $
                     A.cond (i A.== 2) p2Y terminalY
        x1 = pointX segmentIx
        y1 = pointY segmentIx
        x2 = pointX (segmentIx A.+ 1)
        y2 = pointY (segmentIx A.+ 1)
        active = A.cond (layerCode A.>= 0)
                   (segmentIx A.< segmentCount) (A.constant False)
        outputX1 = A.cond active x1 0
        outputY1 = A.cond active y1 0
        outputX2 = A.cond active x2 0
        outputY2 = A.cond active y2 0
        outputLayer = A.cond active layerCode (-1)
        outputActive = A.cond active 1 0
    in A.lift (outputX1, outputY1, outputX2, outputY2, outputLayer, outputActive)

horizontalDiffLength :: Int
horizontalDiffLength = numLayers * gridHeight * (gridWidth + 1)

verticalDiffLength :: Int
verticalDiffLength = numLayers * gridWidth * (gridHeight + 1)

horizontalLineCount :: Int
horizontalLineCount = numLayers * gridHeight

horizontalLineWidth :: Int
horizontalLineWidth = gridWidth + 1

verticalLineCount :: Int
verticalLineCount = numLayers * gridWidth

verticalLineHeight :: Int
verticalLineHeight = gridHeight + 1

segmentEventsAcc :: A.Acc (A.Vector SegmentElt) -> A.Acc (A.Vector SegmentEventElt)
segmentEventsAcc segments =
  A.generate (A.constant (A.Z A.:. (routeBallCount * 6))) $ \ix ->
    let eventIx = A.unindex1 ix
        segmentIx = A.quot eventIx 2
        endpointIx = A.rem eventIx 2
        segment = segments A.! A.index1 segmentIx
        (x1, y1, x2, y2, layerCode, activeCode) =
          A.unlift segment
            :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)
        isHorizontal = y1 A.== y2
        isVertical = x1 A.== x2
        active = activeCode A.== 1
        useHorizontal = A.cond active isHorizontal (A.constant False)
        useVertical = A.cond active isVertical (A.constant False)
        startCoord = A.cond isHorizontal x1 y1
        endCoord = A.cond isHorizontal x2 y2
        increasing = startCoord A.< endCoord
        firstOccupied = A.min startCoord endCoord A.+ A.cond increasing 1 0
        endExclusive = A.max startCoord endCoord A.+ A.cond increasing 1 0
        position = A.cond (endpointIx A.== 0) firstOccupied endExclusive
        delta = A.cond (endpointIx A.== 0) 1 (-1)
        horizontalDestination =
          (((layerCode A.* A.constant gridHeight) A.+ y1)
              A.* A.constant (gridWidth + 1)) A.+ position
        verticalDestination =
          (((layerCode A.* A.constant gridWidth) A.+ x1)
              A.* A.constant (gridHeight + 1)) A.+ position
        hIndex = A.cond useHorizontal horizontalDestination 0
        hDelta = A.cond useHorizontal delta 0
        vIndex = A.cond useVertical verticalDestination 0
        vDelta = A.cond useVertical delta 0
    in A.lift (hIndex, hDelta, vIndex, vDelta)

eventHorizontalIndex :: A.Exp SegmentEventElt -> A.Exp Int
eventHorizontalIndex event = i
  where (i, _, _, _) = A.unlift event :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)

eventHorizontalDelta :: A.Exp SegmentEventElt -> A.Exp Int
eventHorizontalDelta event = d
  where (_, d, _, _) = A.unlift event :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)

eventVerticalIndex :: A.Exp SegmentEventElt -> A.Exp Int
eventVerticalIndex event = i
  where (_, _, i, _) = A.unlift event :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)

eventVerticalDelta :: A.Exp SegmentEventElt -> A.Exp Int
eventVerticalDelta event = d
  where (_, _, _, d) = A.unlift event :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)

horizontalDiffAcc :: A.Acc (A.Vector SegmentEventElt) -> A.Acc (A.Vector Int)
horizontalDiffAcc events =
  A.permute (\new old -> new A.+ old)
    (A.fill (A.constant (A.Z A.:. horizontalDiffLength)) 0)
    (\ix ->
      let event = events A.! ix
          delta = eventHorizontalDelta event
      in A.cond (delta A.== 0) A.Nothing_
           (A.Just_ (A.index1 (eventHorizontalIndex event))))
    (A.map eventHorizontalDelta events)

verticalDiffAcc :: A.Acc (A.Vector SegmentEventElt) -> A.Acc (A.Vector Int)
verticalDiffAcc events =
  A.permute (\new old -> new A.+ old)
    (A.fill (A.constant (A.Z A.:. verticalDiffLength)) 0)
    (\ix ->
      let event = events A.! ix
          delta = eventVerticalDelta event
      in A.cond (delta A.== 0) A.Nothing_
           (A.Just_ (A.index1 (eventVerticalIndex event))))
    (A.map eventVerticalDelta events)

horizontalOccupancyAcc :: A.Acc (A.Vector Int) -> A.Acc (A.Vector Int)
horizontalOccupancyAcc differences =
  A.reshape (A.index1 (A.constant horizontalDiffLength)) scanned
  where
    rows :: A.Acc (A.Array A.DIM2 Int)
    rows = A.reshape
      (A.constant (A.Z A.:. horizontalLineCount A.:. horizontalLineWidth)) differences
    scanned :: A.Acc (A.Array A.DIM2 Int)
    scanned = A.scanl1 (+) rows

verticalOccupancyAcc :: A.Acc (A.Vector Int) -> A.Acc (A.Vector Int)
verticalOccupancyAcc differences =
  A.reshape (A.index1 (A.constant verticalDiffLength)) scanned
  where
    columns :: A.Acc (A.Array A.DIM2 Int)
    columns = A.reshape
      (A.constant (A.Z A.:. verticalLineCount A.:. verticalLineHeight)) differences
    scanned :: A.Acc (A.Array A.DIM2 Int)
    scanned = A.scanl1 (+) columns

traceResourceCount :: Int
traceResourceCount = numLayers * gridHeight * gridWidth

fanoutTraceOccupancyAcc :: A.Acc Fanout -> A.Acc (A.Vector Int)
fanoutTraceOccupancyAcc fanout =
  A.generate (A.constant (A.Z A.:. traceResourceCount)) $ \ix ->
    let resourceIx = A.unindex1 ix
        x = A.rem resourceIx (A.constant gridWidth)
        q = A.quot resourceIx (A.constant gridWidth)
        y = A.rem q (A.constant gridHeight)
        l = A.quot q (A.constant gridHeight)
        horizontalIx =
          (((l A.* A.constant gridHeight) A.+ y)
              A.* A.constant horizontalLineWidth) A.+ x
        verticalIx =
          (((l A.* A.constant gridWidth) A.+ x)
              A.* A.constant verticalLineHeight) A.+ y
        segments = routeSegmentsAcc fanout
        events = segmentEventsAcc segments
        horizontalDifferences = horizontalDiffAcc events
        verticalDifferences = verticalDiffAcc events
        horizontalOccupancy = horizontalOccupancyAcc horizontalDifferences
        verticalOccupancy = verticalOccupancyAcc verticalDifferences
    in horizontalOccupancy A.! A.index1 horizontalIx
         A.+ verticalOccupancy A.! A.index1 verticalIx

viaEventCount :: Int
viaEventCount = routeBallCount * numLayers

viaOccupancyAcc :: A.Acc Fanout -> A.Acc (A.Vector Int)
viaOccupancyAcc fanout =
  A.permute (\new old -> new A.+ old)
    (A.fill (A.constant (A.Z A.:. traceResourceCount)) 0)
    scatterDestination contributions
  where
    flatCandidates = A.reshape (A.index1 (A.constant routeBallCount)) fanout
    contributions = A.generate (A.constant (A.Z A.:. viaEventCount)) $ \ix ->
      let eventIx = A.unindex1 ix
          routeIx = A.quot eventIx (A.constant numLayers)
          layerIx = A.rem eventIx (A.constant numLayers)
          candidate = flatCandidates A.! A.index1 routeIx
          (_, _, _, _, _, candidateLayer) =
            A.unlift candidate
              :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)
          floorLayer = A.cond (candidateLayer A.== (-1)) 0 candidateLayer
      in A.cond (layerIx A.>= floorLayer) 1 0
    scatterDestination ix =
      let eventIx = A.unindex1 ix
          routeIx = A.quot eventIx (A.constant numLayers)
          layerIx = A.rem eventIx (A.constant numLayers)
          candidate = flatCandidates A.! A.index1 routeIx
          (ax, ay, _, _, _, candidateLayer) =
            A.unlift candidate
              :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)
          floorLayer = A.cond (candidateLayer A.== (-1)) 0 candidateLayer
          destination =
            ((layerIx A.* A.constant gridHeight) A.+ ay)
              A.* A.constant gridWidth A.+ ax
          active = layerIx A.>= floorLayer
      in A.cond active (A.Just_ (A.index1 destination)) A.Nothing_

fanoutOccupancyCostAcc :: A.Acc Fanout -> A.Acc (A.Scalar Int)
fanoutOccupancyCostAcc fanout = A.fold (+) 0 $ A.map occupancyPenalty totalOccupancy
  where
    traceOccupancy = fanoutTraceOccupancyAcc fanout
    viasOccupancy = viaOccupancyAcc fanout
    totalOccupancy = A.zipWith (+) traceOccupancy viasOccupancy
    occupancyPenalty :: A.Exp Int -> A.Exp Int
    occupancyPenalty occupancy = A.cond (occupancy A.> 1) (occupancy A.- 1) 0

fanoutOccupancyCostKernel :: Fanout -> A.Scalar Int
fanoutOccupancyCostKernel = CPU.runN fanoutOccupancyCostAcc

fanoutOccupancyCost :: Fanout -> Int
fanoutOccupancyCost fanout = A.indexArray (fanoutOccupancyCostKernel fanout) A.Z

--------------------------------------------------------------------------------
-- Preference loss and combined loss
--------------------------------------------------------------------------------

routePreferenceCostAcc :: A.Exp CandidateElt -> A.Exp Int
routePreferenceCostAcc candidate = A.cond (layerCode A.== (-1)) 0 cost
  where
    (ax, ay, xp, yp, edgeCode, layerCode) =
      A.unlift candidate
        :: (A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int, A.Exp Int)
    ax' = A.constant (gridWidth - 1) A.- ax
    ay' = A.constant (gridHeight - 1) A.- ay
    nearestDistance = A.min ay $ A.min ax' $ A.min ay' ax
    chosenDistance = A.cond (edgeCode A.== 0) ay $
      A.cond (edgeCode A.== 1) ax' $
        A.cond (edgeCode A.== 2) ay' ax
    edgeCost = chosenDistance A.- nearestDistance
    targetPick = A.cond (edgeCode A.== 0) xp $
      A.cond (edgeCode A.== 2) xp yp
    cost = edgeCost A.+ targetPick

fanoutPreferenceCostAcc' :: A.Acc Fanout -> A.Acc (A.Scalar Int)
fanoutPreferenceCostAcc' fanout =
  A.fold (+) 0 $ A.fold (+) 0 $ A.map routePreferenceCostAcc fanout

fanoutPreferenceKernel :: Fanout -> A.Scalar Int
fanoutPreferenceKernel = CPU.runN fanoutPreferenceCostAcc'

fanoutPreferenceCostAcc :: Fanout -> Int
fanoutPreferenceCostAcc fanout = A.indexArray (fanoutPreferenceKernel fanout) A.Z

fanoutLoss :: Fanout -> (Int, Int)
fanoutLoss fanout = (fanoutOccupancyCost fanout, fanoutPreferenceCostAcc fanout)

--------------------------------------------------------------------------------
-- Simulated annealing
--------------------------------------------------------------------------------

proposeFanoutMove :: forall nx ny. (KnownNat nx, KnownNat ny)
                  => BGA nx ny 1 -> Fanout -> StdGen
                  -> Either String (Fanout, StdGen, (Int, Int))
proposeFanoutMove (BGA pinArr) fanout gen0
  | not pinShapeCorrect = Left "BGA array dimensions do not match its type"
  | not fanoutShapeCorrect = Left "Fanout array dimensions do not match BGA dimensions"
  | length pinCodes /= nx * ny = Left "BGA has an incorrect number of pin codes"
  | not (all (isJust . decodePin) pinCodes) = Left "BGA contains an invalid pin code"
  | null signalBalls = Left "Cannot propose a move: BGA has no signal balls"
  | otherwise = do
      ((i, bx, by, pinCode), gen1) <- chooseOne signalBalls gen0
      let oldCandidate = candidates !! i
      (newCandidate, gen2) <- chooseDifferentCandidate bx by pinCode oldCandidate (16 :: Int) gen1
      _ <- validateCandidateForPin bx by pinCode newCandidate
      let candidates' = take i candidates ++ (newCandidate : drop (i + 1) candidates)
          fanout' = A.fromList (A.Z A.:. nx A.:. ny) candidates'
      pure (fanout', gen2, (bx, by))
  where
    nx = natInt @nx
    ny = natInt @ny
    pinCodes = A.toList pinArr
    candidates = A.toList fanout
    coords = [(x, y) | x <- [0 .. nx - 1], y <- [0 .. ny - 1]]
    pinShapeCorrect = case A.arrayShape pinArr of
      A.Z A.:. px A.:. py A.:. pl -> px == nx && py == ny && pl == 1
    fanoutShapeCorrect = case A.arrayShape fanout of
      A.Z A.:. fx A.:. fy -> fx == nx && fy == ny
    signalBalls =
      [ (i, x, y, pinCode)
      | (i, ((x, y), pinCode)) <- zip [0 ..] (zip coords pinCodes)
      , pinCode > 0
      ]
    chooseDifferentCandidate :: Int -> Int -> Int -> CandidateElt -> Int -> StdGen
                            -> Either String (CandidateElt, StdGen)
    chooseDifferentCandidate bx by pinCode oldCandidate attempts gen = do
      (candidate, gen') <- randomCandidate nx ny bx by pinCode gen
      if candidate /= oldCandidate || attempts <= 1
        then Right (candidate, gen')
        else chooseDifferentCandidate bx by pinCode oldCandidate (attempts - 1) gen'

acceptMove :: (Int, Int) -- Current loss: (congestion, preference)
           -> (Int, Int) -- Proposed loss
           -> Double     -- Congestion temperature
           -> Double     -- Preference temperature
           -> StdGen
           -> (Bool, StdGen)
acceptMove (currentC, currentP) (proposedC, proposedP) congestionTemp preferenceTemp gen
  | (proposedC, proposedP) <= (currentC, currentP) = (True, gen)
  | proposedC == currentC =
      acceptByTemperature preferenceTemp (proposedP - currentP) gen
  | otherwise = acceptByTemperature congestionTemp (proposedC - currentC) gen

acceptByTemperature :: Double -> Int -> StdGen -> (Bool, StdGen)
acceptByTemperature temperature delta gen
  | delta <= 0 = (True, gen)
  | temperature <= 0 = (False, gen)
  | otherwise = (u < probability, gen')
  where
    (u, gen') = randomR (0.0 :: Double, 1.0) gen
    probability = exp (negate (fromIntegral delta / temperature))

data AnnealConfig = AnnealConfig
  { annealIterations            :: !Int
  , annealInitialCongestionTemp :: !Double
  , annealFinalCongestionTemp   :: !Double
  , annealInitialPreferenceTemp :: !Double
  , annealFinalPreferenceTemp   :: !Double
  } deriving (Eq, Show)

data AnnealResult = AnnealResult
  { annealBestFanout       :: !Fanout
  , annealBestLoss         :: !(Int, Int)
  , annealFinalFanout      :: !Fanout
  , annealFinalLoss        :: !(Int, Int)
  , annealAcceptedMoves    :: !Int
  , annealIterationsDone   :: !Int
  , annealFinalGenerator   :: !StdGen
  } deriving (Show)

coolingTemperature :: Double -> Double -> Int -> Int -> Double
coolingTemperature initial final step total
  | total <= 1 = initial
  | initial <= 0 || final <= 0 = max 0 (initial + fraction * (final - initial))
  | otherwise = initial * ((final / initial) ** fraction)
  where
    fraction = fromIntegral step / fromIntegral (total - 1)

annealStep :: (KnownNat nx, KnownNat ny)
           => BGA nx ny 1 -> Fanout -> (Int, Int)
           -> Double -> Double -> StdGen
           -> Either String (Fanout, (Int, Int), Bool, (Int, Int), StdGen)
annealStep bga currentFanout currentLoss congestionTemp preferenceTemp gen0 = do
  (proposedFanout, gen1, coordinate) <- proposeFanoutMove bga currentFanout gen0
  let proposedLoss = fanoutLoss proposedFanout
      (accepted, gen2) =
        acceptMove currentLoss proposedLoss congestionTemp preferenceTemp gen1
  if accepted
    then Right (proposedFanout, proposedLoss, True, coordinate, gen2)
    else Right (currentFanout, currentLoss, False, coordinate, gen2)

simulatedAnnealing :: (KnownNat nx, KnownNat ny)
                   => AnnealConfig -> BGA nx ny 1 -> Fanout -> StdGen
                   -> Either String AnnealResult
simulatedAnnealing config bga initialFanout gen0
  | annealIterations config < 0 =
      Left "Number of annealing iterations must be non-negative"
  | otherwise = loop 0 initialFanout initialLoss initialFanout initialLoss 0 gen0
  where
    totalIterations = annealIterations config
    initialLoss = fanoutLoss initialFanout

    loop step currentFanout currentLoss bestFanout bestLoss accepted gen
      | step >= totalIterations =
          Right AnnealResult
            { annealBestFanout = bestFanout
            , annealBestLoss = bestLoss
            , annealFinalFanout = currentFanout
            , annealFinalLoss = currentLoss
            , annealAcceptedMoves = accepted
            , annealIterationsDone = step
            , annealFinalGenerator = gen
            }
      | otherwise = do
          let congestionTemp =
                coolingTemperature (annealInitialCongestionTemp config)
                                   (annealFinalCongestionTemp config)
                                   step totalIterations
              preferenceTemp =
                coolingTemperature (annealInitialPreferenceTemp config)
                                   (annealFinalPreferenceTemp config)
                                   step totalIterations
          (nextFanout, nextLoss, wasAccepted, _, gen') <-
            annealStep bga currentFanout currentLoss congestionTemp preferenceTemp gen
          let accepted' = accepted + if wasAccepted then 1 else 0
              (bestFanout', bestLoss') =
                if nextLoss < bestLoss
                  then (nextFanout, nextLoss)
                  else (bestFanout, bestLoss)
          loop (step + 1) nextFanout nextLoss bestFanout' bestLoss' accepted' gen'

runAnnealingChunks :: (KnownNat nx, KnownNat ny)
                   => Int -> AnnealConfig -> BGA nx ny 1 -> Fanout -> StdGen
                   -> IO (Either String AnnealResult)
runAnnealingChunks chunkSize config bga initialFanout gen0
  | chunkSize <= 0 = pure $ Left "Chunk size must be positive"
  | totalIterations < 0 = pure $ Left "Number of iterations must be non-negative"
  | otherwise = do
      let initialLoss = fanoutLoss initialFanout
      putStrLn $ "Starting loss: " ++ show initialLoss
      runChunks 0 initialFanout initialLoss initialFanout initialLoss 0 gen0
  where
    totalIterations = annealIterations config

    runChunks completed currentFanout currentLoss bestFanout bestLoss acceptedTotal gen
      | completed >= totalIterations =
          pure $ Right AnnealResult
            { annealBestFanout = bestFanout
            , annealBestLoss = bestLoss
            , annealFinalFanout = currentFanout
            , annealFinalLoss = currentLoss
            , annealAcceptedMoves = acceptedTotal
            , annealIterationsDone = completed
            , annealFinalGenerator = gen
            }
      | otherwise = do
          let count = min chunkSize (totalIterations - completed)
              lastStep = completed + count - 1
              chunkConfig = AnnealConfig
                { annealIterations = count
                , annealInitialCongestionTemp =
                    coolingTemperature (annealInitialCongestionTemp config)
                                       (annealFinalCongestionTemp config)
                                       completed totalIterations
                , annealFinalCongestionTemp =
                    coolingTemperature (annealInitialCongestionTemp config)
                                       (annealFinalCongestionTemp config)
                                       lastStep totalIterations
                , annealInitialPreferenceTemp =
                    coolingTemperature (annealInitialPreferenceTemp config)
                                       (annealFinalPreferenceTemp config)
                                       completed totalIterations
                , annealFinalPreferenceTemp =
                    coolingTemperature (annealInitialPreferenceTemp config)
                                       (annealFinalPreferenceTemp config)
                                       lastStep totalIterations
                }
          startNs <- getMonotonicTimeNSec
          case simulatedAnnealing chunkConfig bga currentFanout gen of
            Left err -> pure $ Left err
            Right result -> do
              endNs <- getMonotonicTimeNSec
              let elapsedSeconds = fromIntegral (endNs - startNs) / 1.0e9 :: Double
                  iterationsPerSecond = fromIntegral count / max elapsedSeconds 1.0e-9
                  currentFanout' = annealFinalFanout result
                  currentLoss' = annealFinalLoss result
                  chunkBestFanout = annealBestFanout result
                  chunkBestLoss = annealBestLoss result
                  (bestFanout', bestLoss') =
                    if chunkBestLoss < bestLoss
                      then (chunkBestFanout, chunkBestLoss)
                      else (bestFanout, bestLoss)
                  acceptedTotal' = acceptedTotal + annealAcceptedMoves result
                  completed' = completed + count
              putStrLn $
                "Iteration " ++ show completed' ++ "/" ++ show totalIterations
                ++ " | chunk accepted " ++ show (annealAcceptedMoves result)
                ++ "/" ++ show count ++ " | current loss " ++ show currentLoss'
                ++ " | best loss " ++ show bestLoss'
                ++ printf " | %.2f s (%.1f iterations/s)"
                            elapsedSeconds iterationsPerSecond
              runChunks completed' currentFanout' currentLoss' bestFanout'
                bestLoss' acceptedTotal' (annealFinalGenerator result)

--------------------------------------------------------------------------------
-- BGA utilities
--------------------------------------------------------------------------------

mkBGA :: forall nx ny nl. (KnownNat nx, KnownNat ny, KnownNat nl)
      => [Int] -> BGA nx ny nl
mkBGA xs
  | length xs /= nx * ny * nl = error "Incorrect number of grid cells"
  | otherwise = BGA $ A.fromList (A.Z A.:. nx A.:. ny A.:. nl) xs
  where
    nx = natInt @nx
    ny = natInt @ny
    nl = natInt @nl

validatePinout :: BGA nx ny 1 -> Bool
validatePinout (BGA arr) = all (\n -> n >= 0 && n <= numSignalTypes) (A.toList arr)

randomPinout :: forall nx ny. (KnownNat nx, KnownNat ny) => IO (BGA nx ny 1)
randomPinout = do
  gen <- newStdGen
  let nx = natInt @nx
      ny = natInt @ny
      values = take (nx * ny) (randomRs (0, numSignalTypes) gen :: [Int])
      arr = mkBGA @nx @ny @1 values
  if validatePinout arr then pure arr else error "Invalid Pinout"

showBGA :: forall nx ny nl. (KnownNat nx, KnownNat ny, KnownNat nl)
        => BGA nx ny nl -> IO ()
showBGA (BGA arr) = do
  let nx = natInt @nx
      ny = natInt @ny
      nl = natInt @nl
      values = V.fromList (A.toList arr)
      cell :: Int -> Int -> Int -> Int
      cell x y l = values V.! ((x * ny + y) * nl + l)
  forM_ [0 .. nl - 1] $ \l -> do
    putStrLn $ "\nLayer M" ++ show (l + 1)
    putStrLn $ "y=   " ++ concat [printf "%6d" y | y <- [0 .. ny - 1]]
    putStrLn ""
    forM_ [0 .. nx - 1] $ \x ->
      putStrLn $ printf "x=%-3d" x ++ concat
        [printf "%6d" (cell x y l) | y <- [0 .. ny - 1]]
