module Main
  ( main
  ) where

import Algorithms.Teleport (teleport)
import Algorithms.Adder (adder)
import Algorithms.Deutsch (deutsch)

import Gates

teleportEx :: IO ()
teleportEx = do
  v <- virtFromR <$> mkQR (ketMinus &* ket0 &* ket0)

  teleport v
  printVirt v

adderEx :: IO ()
adderEx = do
  v <- virtFromR <$> mkQR (ket0 &* ket0 &* ket0 &* ket0)

  adder v
  printVirt v

deutschEx :: IO ()
deutschEx = do
  v <- virtFromR <$> mkQR (ket0 &* ket1)

  res <- deutsch v id
  print res
  if res
    then putStrLn "Balanced"
    else putStrLn "Constant"


main :: IO ()
main = deutschEx