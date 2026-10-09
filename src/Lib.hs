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

import           Control.Monad (forM_)
import           Text.Printf (printf)
import qualified Data.Vector as V
import           Data.Proxy (Proxy(..))
import           GHC.TypeNats (Nat, KnownNat, natVal)
import           System.Random (newStdGen, randomRs)

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

type NX = 20
type NY = 10
type NL = 5
type NS = 12

type Layer = Int
type Slot  = Int
type Index = Int
type Offset = Int
type Coord = (Slot, Slot, Layer)

newtype BGA (nx :: Nat) (ny :: Nat) (nl :: Nat) = BGA (A.Array A.DIM3 Int)

natInt :: forall n. KnownNat n => Int
natInt = fromIntegral (natVal (Proxy @n))

numLayers :: Int
numLayers = natInt @NL
gridWidth :: Int
gridWidth = natInt @NX * natInt @NS
gridHeight :: Int
gridHeight = natInt @NY * natInt @NS

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
decodePin 0                               = Just Ground
decodePin n | n >= 1 && n <= (natInt @NL) = Just $ Signal $ toEnum (fromEnum (minBound :: SignalType) + n - 1)
            | otherwise                   = Nothing

layerPolicyRows :: [[Int]]
                  --  M1  M2  M3  M4  M5  M6
layerPolicyRows = [ [ 0,  1,  1,  1,  0,  0 ]   -- S
                  , [ 1,  1,  0,  0,  0,  0 ]   -- LR
                  , [ 0,  0,  0,  0,  1,  0 ]   -- BC
                  , [ 0,  0,  0,  0,  1,  0 ] ] -- I

layerPolicy :: A.Array A.DIM2 Int
layerPolicy = A.fromList (A.Z A.:. length layerPolicyRows A.:. numLayers) (concat layerPolicyRows)

layerAllowed :: SignalType -> Layer -> Bool
layerAllowed signal layer | layer < 0 || layer >= numLayers = False
                          | otherwise                       = layerPolicyRows !! fromEnum signal !! layer == 1

type Candidate = A.Array A.DIM3 Int

data Segment = Segment { x0    :: Int
                       , y0    :: Int
                       , x1    :: Int
                       , y1    :: Int
                       , layer :: Int
                       } deriving (Eq, Show)

data RouteGeometry = Direct Segment
                   | Jogged Segment Segment Segment
  deriving (Eq, Show)

resourceIndex :: Coord -> Maybe Int
resourceIndex (x, y, l) | gridWidth <= 0 || gridHeight <= 0 || numLayers <= 0 = Nothing
                        | x < 0 || x >= gridWidth                             = Nothing
                        | y < 0 || y >= gridHeight                            = Nothing
                        | l < 0 || l >= numLayers                             = Nothing
                        | otherwise                                           = Just (((x * gridHeight) + y) * numLayers + l)

resourceCoordinates :: Int -> Maybe Coord
resourceCoordinates r | r < 0 || r >= gridWidth * gridHeight * numLayers = Nothing
                      | otherwise                                        = Just (x, y, l)
  where
    (xy, l) = r `divMod` numLayers
    (x, y)  = xy `divMod` gridHeight

mkBGA :: forall nx ny nl. (KnownNat nx, KnownNat ny, KnownNat nl) => [Int] -> (BGA nx ny nl)
mkBGA xs | length xs /= nx * ny * nl = error "Incorrect number of grid cells"
         | otherwise                 = BGA $ A.fromList (A.Z A.:. nx A.:. ny A.:. nl) xs
  where
    nx = natInt @nx
    ny = natInt @ny
    nl = natInt @nl

randomPinout :: forall nx ny. (KnownNat nx, KnownNat ny) => IO (BGA nx ny 1)
randomPinout = do
    gen <- newStdGen
    let nx     = natInt @nx
        ny     = natInt @ny
        values = take (nx * ny) (randomRs (0, natInt @NL) gen :: [Int])
        arr = mkBGA @nx @ny @1 values
    if validatePinout arr then pure arr else error "Invalid Pinout"

validatePinout :: BGA nx ny 1 -> Bool
validatePinout (BGA arr) = all (\n -> n >= 0 && n <= (natInt @NL)) $ A.toList arr

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
