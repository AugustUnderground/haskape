{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib

main :: IO ()
main = do
  -- let grid = mkBGA @NX @NY @NL [0 .. (natInt @NX) * (natInt @NY) * (natInt @NL) - 1]
  pinout <- randomPinout @NX @NY
  fanout <- randomFanout @NX @NY pinout

  -- showBGA pinout
  -- print (A.arrayShape fanout)
  -- print (take 10 (A.toList fanout))

  -- case validateFanout pinout fanout of
  --   Right ()    -> putStrLn "Fanout is legal"
  --   Left errors -> do
  --     putStrLn "Fanout is illegal:"
  --     mapM_ putStrLn errors
  let score = fanoutClashCount fanout

  putStrLn $ "Fanout Score: " ++ show score
  pure ()
