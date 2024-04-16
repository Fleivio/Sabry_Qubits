module Main
  ( main
  ) where

import Gates

main :: IO ()
main = do
  v <- virtFromList [((((True, True), True), False), 1)]
  adder v
  (((a, b), c), d) <- observeVV v
  putStrLn $ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
