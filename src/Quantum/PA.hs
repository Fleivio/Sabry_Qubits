module Quantum.PA (paToString, PA, squareModulus, addPA, module Data.Complex) where

import Data.Complex ( imagPart, magnitude, realPart, Complex(..) ) 
import Text.Printf

type PA = Complex Double 

paToString :: PA -> String
paToString pa 
    | imagPart pa < 0  = printf "(%.1f %.1f⋅i)" (realPart pa) (imagPart pa)
    | imagPart pa > 0  = printf "(%.1f + %.1f⋅i)" (realPart pa) (imagPart pa)
    | otherwise        = printf "%.1f" (realPart pa)

squareModulus :: PA -> Double
squareModulus = (**2) . magnitude

addPA :: a -> a -> Complex a
addPA = (:+)