{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import qualified Data.Array.Accelerate             as A

main :: IO ()
main = do
  -- let grid = mkBGA @NX @NY @NL [0 .. (natInt @NX) * (natInt @NY) * (natInt @NL) - 1]
  grid <- randomPinout @NX @NY
  showBGA grid
