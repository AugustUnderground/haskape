{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module Lib
--     ( showBGA
--     ) where

module Lib where

import           Data.Maybe (isJust)
import           Control.Monad (forM_)
import           Text.Printf (printf)
import qualified Data.Vector as V
import           Data.Proxy (Proxy(..))
import           GHC.TypeNats (Nat, KnownNat, natVal)
import           System.Random (StdGen, newStdGen, randomRs, randomR)

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU

data SignalType = A | B | C | D
  deriving (Eq, Ord, Show, Enum, Bounded)

data Edge = North | East | South | West
  deriving (Eq, Ord, Show, Enum, Bounded)

data JogMode = JogNone | JogLeft | JogRight
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
type Coord  = (Slot, Slot, Layer)

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

slotStep :: Int
slotStep = 1

slotOffset :: Slot -> Maybe Offset
slotOffset choice | choice < 0  = Nothing
                  | choice == 0 = Just 0
                  | odd choice  = Just ((choice + 1) `div` 2)
                  | otherwise   = Just (negate (choice `div` 2))

slotTarget :: Int -> Int -> Int -> Maybe Int
slotTarget axisSize source choice | axisSize <= 0                    = Nothing
                                  | source < 0 || source >= axisSize = Nothing
                                  | otherwise                        = do
      offset <- slotOffset choice
      let ns         = natInt @NS
          anchor     = anchorSlot ns source
          target     = anchor + offset
          totalSlots = axisSize * ns
      if ns > 0 && target >= 0 && target < totalSlots
        then Just target else Nothing

anchorSlot :: Int -> Index -> Slot
anchorSlot ns coordinate | ns <= 0   = error "NS must be positive"
                         | otherwise = coordinate * ns + (ns - 1) `div` 2

viaAnchor' :: Index -> Index -> (Int, Int)
viaAnchor' x y = (anchorSlot (natInt @NS) x, anchorSlot (natInt @NS) y)

viaAnchor :: Index -> Index -> Layer -> Coord
viaAnchor x' y' l = (x, y, l)
  where
    (x, y) = viaAnchor' x' y'

encodePin :: Pin -> Int
encodePin Ground     = 0
encodePin (Signal s) = fromEnum s - fromEnum (minBound :: SignalType) + 1

decodePin :: Int -> Maybe Pin
decodePin 0 = Just Ground
decodePin n | n >= 1 && n <= numSignalTypes = Just $ Signal $ toEnum (fromEnum (minBound :: SignalType) + n - 1)
            | otherwise                     = Nothing

layerPolicyRows :: [[Int]] --  M1  M2  M3  M4  M5
layerPolicyRows = [ [ 0,  1,  1,  1,  0 ]   -- A
                  , [ 1,  1,  0,  0,  0 ]   -- B
                  , [ 0,  0,  0,  0,  1 ]   -- C
                  , [ 0,  0,  0,  0,  1 ] ] -- D

layerPolicy :: A.Array A.DIM2 Int
layerPolicy = A.fromList (A.Z A.:. length layerPolicyRows A.:. numLayers) (concat layerPolicyRows)

layerAllowed :: SignalType -> Layer -> Bool
layerAllowed signal layer' | layer' < 0 || layer' >= numLayers = False
                           | otherwise                         = layerPolicyRows !! fromEnum signal !! layer' == 1

edgePolicyRows :: [[Int]]
edgePolicyRows = [ [1, 0, 1, 0] -- A: North / South
                 , [1, 0, 1, 0] -- B: North / South
                 , [0, 1, 0, 1] -- C: East / West
                 , [0, 1, 0, 1] -- D: East / West
                 ]

edgePolicy :: A.Array A.DIM2 Int
edgePolicy = A.fromList (A.Z A.:. numSignalTypes A.:. 4) (concat edgePolicyRows)

edgeAllowed :: SignalType -> Edge -> Bool
edgeAllowed signal edge' = edgePolicyRows !! fromEnum signal !! fromEnum edge' == 1

maxSlotChoices :: Int
maxSlotChoices = 3 * natInt @NS
validSlotPicks :: Int -> Int -> Int -> [Int]
validSlotPicks axisSize source limit = [ pick | pick <- [0 .. max 0 limit - 1]
                                       , isJust (slotTarget axisSize source pick) ]

newtype Path (n :: Nat) = Path (A.Array A.DIM1 (Int, Int))
  deriving Show

type DirectPath = Path 2
type JoggedPath = Path 4

mkPath :: forall n. KnownNat n => [(Int, Int)] -> (Path n)
mkPath points | length points /= natInt @n = error "Incorrect number of route points"
              | otherwise                  = Path $ A.fromList (A.Z A.:. natInt @n) points

pathPoints :: Path n -> [(Int, Int)]
pathPoints (Path arr) = A.toList arr

resourceIndex :: Coord -> Maybe Int
resourceIndex (x, y, l) | gridWidth <= 0 || gridHeight <= 0 || numLayers <= 0 = Nothing
                        | x < 0 || x >= gridWidth                             = Nothing
                        | y < 0 || y >= gridHeight                            = Nothing
                        | l < 0 || l >= numLayers                             = Nothing
                        | otherwise                                           = Just (((x * gridHeight) + y) * numLayers + l)

type CandidateElt = ( Int  -- anchor X
                    , Int  -- anchor Y
                    , Int  -- X slot pick
                    , Int  -- Y slot pick
                    , Int  -- edge
                    , Int  -- layer
                    )

type Fanout = A.Array A.DIM2 CandidateElt

data RouteCandidate = RouteCandidate { anchorX :: !Int
                                     , anchorY :: !Int
                                     , xPick   :: !Int
                                     , yPick   :: !Int
                                     , edge    :: !Int
                                     , layer   :: !Int
                                     } deriving (Eq, Show)

fromCandidateElt :: CandidateElt -> RouteCandidate
fromCandidateElt (ax, ay, xp, yp, e, l) = RouteCandidate ax ay xp yp e l

toCandidateElt :: RouteCandidate -> CandidateElt
toCandidateElt c = (anchorX c , anchorY c , xPick c , yPick c , edge c , layer c)

resourceCoordinates :: Int -> Maybe Coord
resourceCoordinates r | r < 0 || r >= gridWidth * gridHeight * numLayers = Nothing
                      | otherwise                                        = Just (x, y, l)
  where
    (xy, l) = r `divMod` numLayers
    (x, y)  = xy `divMod` gridHeight

slotPickForTarget :: Int -> Int -> Int
slotPickForTarget anchor target = case compare (target - anchor) 0 of
    EQ -> 0
    GT -> 2 * (target - anchor) - 1
    LT -> 2 * (anchor - target)

type Point2 = (Int, Int)

data CandidateGeometry = GroundVia !Point2
                       | SignalTrace !Point2 !Layer !Edge ![Point2]
  deriving (Eq, Show)

candidateAnchor :: CandidateGeometry -> Point2
candidateAnchor (GroundVia p)          = p
candidateAnchor (SignalTrace p _ _ _)  = p

viaFloor :: CandidateGeometry -> Layer
viaFloor (GroundVia _)          = 0
viaFloor (SignalTrace _ l _ _)  = l

decodeEdgeCode :: Int -> Either String Edge
decodeEdgeCode 0 = Right North
decodeEdgeCode 1 = Right East
decodeEdgeCode 2 = Right South
decodeEdgeCode 3 = Right West
decodeEdgeCode n = Left $ "Invalid edge code: " ++ show n

validAnchorCoordinate :: Int -> Int -> Bool
validAnchorCoordinate ballCount coordinate | ns <= 0   = False
                                           | otherwise = coordinate >= firstAnchor &&
                                                         coordinate < ballCount * ns &&
                                                         (coordinate - firstAnchor) `mod` ns == 0
  where
    ns = natInt @NS
    firstAnchor = (ns - 1) `div` 2

strictlyBetween :: Int -> Int -> Int -> Bool
strictlyBetween x a b = min a b < x && x < max a b

resolvePick :: String -> Int -> Int -> Slot -> Either String Int
resolvePick label extent anchor pick
    | maxSlotChoices <= 0                = Left "maxSlotChoices must be positive"
    | pick < 0 || pick >= maxSlotChoices = Left $ label ++ " pick outside search limit: " ++ show pick
    | otherwise                          = case slotOffset pick of
                                              Nothing -> Left $ "Invalid " ++ label ++ " pick: " ++ show pick
                                              Just offset -> let target = anchor + offset
                                                              in if target >= 0 && target < extent
                                                                    then Right target
                                                                    else Left $ label ++ " target outside grid: " ++ show target

candidatePoints :: Edge -> Int -> Int -> Int -> Int -> Either String [Point2]
candidatePoints North ax ay xp yp | xp == 0   = Right [(ax, ay), (ax, 0)]
                                  | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      jy <- resolvePick "Y" gridHeight ay yp
      if strictlyBetween jy ay 0
        then Right [ (ax, ay) , (ax, jy) , (tx, jy) , (tx, 0) ]
        else Left "North jog must lie strictly between source and edge"
candidatePoints South ax ay xp yp | xp == 0   = Right [(ax, ay), (ax, gridHeight - 1)]
                                  | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      jy <- resolvePick "Y" gridHeight ay yp
      let edgeY = gridHeight - 1
      if strictlyBetween jy ay edgeY
        then Right [ (ax, ay) , (ax, jy) , (tx, jy) , (tx, edgeY) ]
        else Left "South jog must lie strictly between source and edge"
candidatePoints East ax ay xp yp | yp == 0   = Right [(ax, ay), (gridWidth - 1, ay)]
                                 | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      ty <- resolvePick "Y" gridHeight ay yp
      let edgeX = gridWidth - 1
      if strictlyBetween tx ax edgeX
        then Right [ (ax, ay) , (tx, ay) , (tx, ty) , (edgeX, ty) ]
        else Left "East jog must lie strictly between source and edge"
candidatePoints West ax ay xp yp | yp == 0   = Right [(ax, ay), (0, ay)]
                                 | otherwise = do
      tx <- resolvePick "X" gridWidth ax xp
      ty <- resolvePick "Y" gridHeight ay yp
      if strictlyBetween tx ax 0
        then Right [ (ax, ay) , (tx, ay) , (tx, ty) , (0, ty) ]
        else Left "West jog must lie strictly between source and edge"

candidateGeometry :: CandidateElt -> Either String CandidateGeometry
candidateGeometry (ax, ay, xp, yp, e, l)
    | not (validAnchorCoordinate (natInt @NX) ax) = Left $ "Invalid anchor X: " ++ show ax
    | not (validAnchorCoordinate (natInt @NY) ay) = Left $ "Invalid anchor Y: " ++ show ay
    | l == -1                                     = if (xp, yp, e) == (-1, -1, -1)
                                                      then Right $ GroundVia (ax, ay)
                                                      else Left "Malformed ground candidate"
    | l < 0 || l >= numLayers                     = Left $ "Invalid layer: " ++ show l
    | otherwise                                   = do
        edge'  <- decodeEdgeCode e
        points <- candidatePoints edge' ax ay xp yp
        Right $ SignalTrace (ax, ay) l edge' points

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
pathsIntersect a b = or [ segmentsIntersect sa sb
                        | sa <- pathSegments a , sb <- pathSegments b ]

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
                            Just (routeLayer, points) -> routeLayer >= viaFloor via && pointOnPath (candidateAnchor via) points
                            Nothing                   -> False

candidateGeometriesConflict :: CandidateGeometry -> CandidateGeometry -> Bool
candidateGeometriesConflict a b =  candidateAnchor a == candidateAnchor b
                                || sameLayerTraceConflict
                                || traceHitsVia a b
                                || traceHitsVia b a
  where
    sameLayerTraceConflict = case (traceInfo a, traceInfo b) of
                                (Just (la, pa), Just (lb, pb)) -> la == lb && pathsIntersect pa pb
                                _                              -> False

candidatePairLegal :: CandidateElt -> CandidateElt -> Either String Bool
candidatePairLegal a b = do
    ga <- candidateGeometry a
    gb <- candidateGeometry b
    pure $ not (candidateGeometriesConflict ga gb)

validateCandidateForPin :: Int -> Int -> Int -> CandidateElt -> Either String CandidateGeometry
validateCandidateForPin bx by pinCode candidate = do
  pin      <- maybe (Left $ "Invalid pin code: " ++ show pinCode) Right (decodePin pinCode)
  geometry <- candidateGeometry candidate

  if candidateAnchor geometry /= expectedAnchor
    then Left $ "Candidate anchor does not match BGA coordinate " ++ show (bx, by)
    else pure ()

  case (pin, geometry) of
    (Ground, GroundVia _)                    -> Right geometry
    (Ground, _)                              -> Left "Ground pin must use the ground candidate sentinel"
    (Signal _, GroundVia _)                  -> Left "Signal pin cannot use the ground candidate sentinel"
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
pairwiseConflicts []                   = []
pairwiseConflicts ((coordA, a) : rest) = [ "Route conflict between BGA " ++ show coordA ++ " and BGA " ++ show coordB
                                         | (coordB, b) <- rest , candidateGeometriesConflict a b
                                         ] ++ pairwiseConflicts rest

validateFanout :: forall nx ny. (KnownNat nx, KnownNat ny) => BGA nx ny 1 -> Fanout -> Either [String] ()
validateFanout (BGA pinArr) fanout
    | not pinShapeCorrect          = Left ["BGA array dimensions do not match its type"]
    | not fanoutShapeCorrect       = Left ["Fanout array dimensions do not match BGA dimensions"]
    | length pinCodes /= nx * ny   = Left ["BGA has an incorrect number of pin codes"]
    | length candidates /= nx * ny = Left ["Fanout has an incorrect number of candidates"]
    | not (null localErrors)       = Left localErrors
    | not (null conflicts)         = Left conflicts
    | otherwise                    = Right ()
  where
    nx                 = natInt @nx
    ny                 = natInt @ny
    pinShapeCorrect    = case A.arrayShape pinArr of
        A.Z A.:. px A.:. py A.:. pl -> px == nx && py == ny && pl == 1
    fanoutShapeCorrect = case A.arrayShape fanout of
        A.Z A.:. fx A.:. fy -> fx == nx && fy == ny
    pinCodes           = A.toList pinArr
    candidates         = A.toList fanout
    coords             = [ (x, y) | x <- [0 .. nx - 1] , y <- [0 .. ny - 1] ]
    checked            = [ ((x, y), validateCandidateForPin x y pinCode candidate)
                         | ((x, y), (pinCode, candidate)) <- zip coords (zip pinCodes candidates) ]
    localErrors        = [ "BGA " ++ show coord ++ ": " ++ err
                         | (coord, Left err) <- checked ]
    validCandidates    = [ (coord, geometry)
                         | (coord, Right geometry) <- checked ]
    conflicts          = pairwiseConflicts validCandidates

randomOtherCoordinate :: Int -> Int -> StdGen -> (Int, StdGen)
randomOtherCoordinate extent anchor gen = (target, gen')
  where
  (i, gen') = randomR (0, extent - 2) gen
  target = if i >= anchor then i + 1 else i

randomInteriorCoordinate :: Int -> Int -> StdGen -> Maybe (Int, StdGen)
randomInteriorCoordinate source boundary gen | lo > hi   = Nothing
                                             | otherwise = Just (p, gen')
  where
    lo = min source boundary + 1
    hi = max source boundary - 1
    (p, gen') = randomR (lo, hi) gen

chooseOne :: [a] -> StdGen -> Either String (a, StdGen)
chooseOne [] _   = Left "Cannot choose from an empty list"
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
    | pinCode == 0                            = Right ((ax, ay, -1, -1, -1, -1), gen0)
    | pinCode < 1 || pinCode > numSignalTypes = Left $ "Invalid pin code: " ++ show pinCode
    | null allowedLayers                      = Left "Signal type has no eligible metal layers"
    | null allowedEdges                       = Left "Signal type has no eligible edges"
    | otherwise                               = do
        (edge', g1)  <- chooseOne allowedEdges gen0
        (layer', g2) <- chooseOne allowedLayers g1
        let (kind', g3) = randomR (0 :: Int, 1 :: Int) g2
        if kind' == 0
          then makeDirect edge' layer' g3
          else makeJog edge' layer' g3
  where
    ns            = natInt @NS
    gridX         = nx * ns
    gridY         = ny * ns
    ax            = anchorSlot ns bx
    ay            = anchorSlot ns by
    signal        = toEnum (pinCode - 1) :: SignalType
    allowedLayers = [ l | l <- [0 .. numLayers - 1], layerAllowed signal l ]
    allowedEdges  = [ e | e <- [North, East, South, West], edgeAllowed signal e ]
    makeDirect edge'' layer'' gen = Right ((ax, ay, 0, 0, fromEnum edge'', layer''), gen)
    makeJog edge'' layer'' gen =
        let isNS           = edge'' == North || edge'' == South
            targetExtent   = if isNS then gridX else gridY
            targetAnchor   = if isNS then ax else ay
            approachExtent = if isNS then gridY else gridX
            approachAnchor = if isNS then ay else ax
            boundary       = case edge'' of North -> 0
                                            East  -> gridX - 1
                                            South -> gridY - 1
                                            West  -> 0
            targetPicks    = [ p | p <- [1 .. maxSlotChoices - 1]
                                 , isJust (pickCoordinate targetExtent targetAnchor p) ]
            approachPicks  = [ p | p <- [1 .. maxSlotChoices - 1]
                                 , Just coord <- [pickCoordinate approachExtent approachAnchor p]
                                 , strictlyBetween coord approachAnchor boundary ]
         in if null targetPicks || null approachPicks
               then makeDirect edge'' layer'' gen
               else do
                  (targetPick, g1)   <- chooseOne targetPicks gen
                  (approachPick, g2) <- chooseOne approachPicks g1
                  let (xp, yp) = if isNS then (targetPick, approachPick)
                                         else (approachPick, targetPick)
                  Right ((ax, ay, xp, yp, fromEnum edge'', layer''), g2)

randomFanoutWithGen :: forall nx ny. (KnownNat nx, KnownNat ny) => BGA nx ny 1 -> StdGen
                    -> Either String (A.Array A.DIM2 CandidateElt, StdGen)
randomFanoutWithGen (BGA pinArr) gen0 = do
  if length pinCodes /= nx * ny
    then Left "Pinout has incorrect dimensions"
    else do
      (choices, gen1) <- go (zip coords pinCodes) gen0 []
      let fanout = A.fromList (A.Z A.:. nx A.:. ny) choices
      pure (fanout, gen1)
  where
    go []                      gen acc = Right (reverse acc, gen)
    go (((x,y), pinCode):rest) gen acc = do
        (choice, gen') <- randomCandidate (natInt @nx) (natInt @ny) x y pinCode gen
        go rest gen' (choice : acc)
    nx       = natInt @nx
    ny       = natInt @ny
    pinCodes = A.toList pinArr
    coords   = [ (x, y) | x <- [0 .. nx - 1] , y <- [0 .. ny - 1] ]

randomFanout :: forall nx ny. (KnownNat nx, KnownNat ny) => BGA nx ny 1 -> IO (A.Array A.DIM2 CandidateElt)
randomFanout bga = do
  gen <- newStdGen
  case randomFanoutWithGen @nx @ny bga gen of
    Left err          -> fail err
    Right (fanout, _) -> pure fanout

mkBGA :: forall nx ny nl. (KnownNat nx, KnownNat ny, KnownNat nl) => [Int] -> (BGA nx ny nl)
mkBGA xs | length xs /= nx * ny * nl = error "Incorrect number of grid cells"
         | otherwise                 = BGA $ A.fromList (A.Z A.:. nx A.:. ny A.:. nl) xs
  where
    nx = natInt @nx
    ny = natInt @ny
    nl = natInt @nl

validatePinout :: BGA nx ny 1 -> Bool
validatePinout (BGA arr) = all (\n -> n >= 0 && n <= numSignalTypes) (A.toList arr)

randomPinout :: forall nx ny. (KnownNat nx, KnownNat ny) => IO (BGA nx ny 1)
randomPinout = do
    gen <- newStdGen
    let nx     = natInt @nx
        ny     = natInt @ny
        values = take (nx * ny) (randomRs (0, numSignalTypes) gen :: [Int])
        arr    = mkBGA @nx @ny @1 values
    if validatePinout arr then pure arr else error "Invalid Pinout"

edgeDistanceToBoundary :: Edge -> Int -> Int -> Int
edgeDistanceToBoundary North _  ay = ay
edgeDistanceToBoundary East  ax _  = gridWidth  - 1 - ax
edgeDistanceToBoundary South _  ay = gridHeight - 1 - ay
edgeDistanceToBoundary West  ax _  = ax

routePreferenceCost :: CandidateElt -> Either String Int
routePreferenceCost candidate@(ax, ay, xp, yp, _, _) = do
    geometry <- candidateGeometry candidate
    case geometry of GroundVia _                  -> Right 0
                     SignalTrace _ _ chosenEdge _ ->
                        let distances = [ edgeDistanceToBoundary e ax ay
                                        | e <- [North, East, South, West] ]
                            edgeCost = edgeDistanceToBoundary chosenEdge ax ay
                                     - minimum distances
                            slotCost = case chosenEdge of North -> xp
                                                          South -> xp
                                                          East  -> yp
                                                          West  -> yp
                         in Right (edgeCost + slotCost)

candidateScore :: CandidateElt -> [CandidateElt] -> Either String (Int, Int)
candidateScore candidate others = do
    preference <- routePreferenceCost candidate
    geometry   <- candidateGeometry candidate
    otherGeometries <- traverse candidateGeometry others
    let clashes = length $ filter (candidateGeometriesConflict geometry) otherGeometries
    pure (clashes, preference)

routePreferenceCostAcc :: A.Exp CandidateElt -> A.Exp Int
routePreferenceCostAcc candidate = A.cond (layerCode A.== (-1)) 0 cost
  where
    (ax, ay, xp, yp, edgeCode, layerCode) = A.unlift candidate :: ( A.Exp Int, A.Exp Int, A.Exp Int
                                                                  , A.Exp Int, A.Exp Int, A.Exp Int )
    ax'                                   = A.constant (gridWidth  - 1) A.- ax
    ay'                                   = A.constant (gridHeight - 1) A.- ay
    nearestDistance                       = A.min ay $ A.min ax' $ A.min ay' ax
    chosenDistance                        = A.cond (edgeCode A.== 0) ay
                                          $ A.cond (edgeCode A.== 1) ax'
                                          $ A.cond (edgeCode A.== 2) ay' ax
    edgeCost                              = chosenDistance A.- nearestDistance
    targetPick                            = A.cond (edgeCode A.== 0) xp
                                          $ A.cond (edgeCode A.== 2) xp yp
    cost                                  = edgeCost A.+ targetPick

fanoutPreferenceCostAcc' :: A.Acc (A.Array A.DIM2 CandidateElt) -> A.Acc (A.Scalar Int)
fanoutPreferenceCostAcc' fanout = A.fold (+) 0 $ A.fold (+) 0 $ A.map routePreferenceCostAcc fanout

fanoutPreferenceKernel :: Fanout -> A.Scalar Int
fanoutPreferenceKernel = CPU.runN fanoutPreferenceCostAcc'

fanoutPreferenceCostAcc :: Fanout -> Int
fanoutPreferenceCostAcc fanout = A.indexArray (fanoutPreferenceKernel fanout) A.Z

fanoutScore :: Fanout -> Either String (Int, Int)
fanoutScore fanout = do
    geometries <- traverse candidateGeometry candidates
    let clashes    = countConflictPairs geometries
        preference = fanoutPreferenceCostAcc fanout
    pure (clashes, preference)
  where
    candidates = A.toList fanout
    countConflictPairs :: [CandidateGeometry] -> Int
    countConflictPairs      []  = 0
    countConflictPairs (g : gs) = length (filter (candidateGeometriesConflict g) gs)
                                + countConflictPairs gs

fanoutClashCount :: Fanout -> Either String Int
fanoutClashCount fanout = do
    geometries <- traverse candidateGeometry (A.toList fanout)
    pure (countConflictPairs geometries)
  where
    countConflictPairs      []  = 0
    countConflictPairs (g : gs) = length (filter (candidateGeometriesConflict g) gs)
                                + countConflictPairs gs

showBGA :: forall nx ny nl. (KnownNat nx, KnownNat ny, KnownNat nl) => BGA nx ny nl -> IO ()
showBGA (BGA arr) = do
  let nx     = natInt @nx
      ny     = natInt @ny
      nl     = natInt @nl
      values = V.fromList (A.toList arr)
      cell :: Int -> Int -> Int -> Int
      cell x y l = values V.! ((x * ny + y) * nl + l)

  forM_ [0 .. nl - 1] $ \l -> do
    putStrLn $ "\nLayer M" ++ show (l + 1)
    putStrLn $ "y=   " ++ concat [ printf "%6d" y | y <- [0 .. ny - 1] ]
    putStrLn ""
    forM_ [0 .. nx - 1] $ \x -> do
      putStrLn $ printf "x=%-3d" x ++ concat [ printf "%6d" (cell x y l) | y <- [0 .. ny - 1] ]
