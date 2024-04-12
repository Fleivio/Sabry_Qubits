module Main(main) where

import Quantum.Value

main :: IO ()
main = let qv = mkQV [(False, 0), (True, 1)]
        in putStrLn $ showQV qv
