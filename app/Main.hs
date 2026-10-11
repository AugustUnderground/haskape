{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import System.Random (newStdGen)

main :: IO ()
main = do
  pinout <- randomPinout @NX @NY
  fanout <- randomFanout @NX @NY pinout
  gen    <- newStdGen

  let config = AnnealConfig { annealIterations            = 1000
                            , annealInitialCongestionTemp = 2000
                            , annealFinalCongestionTemp   = 0.1
                            , annealInitialPreferenceTemp = 2000
                            , annealFinalPreferenceTemp   = 0.1 }
  _ <- runAnnealingChunks 100 config pinout fanout gen

  pure ()
