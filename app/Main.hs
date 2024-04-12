module Main(main) where

import Quantum.PA

main :: IO ()
main = print $ squareModulus (3.0 :+ 4.0)
