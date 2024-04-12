module Quantum.PA (PA, showPA, showPAMultiplicative, squareModulus, addPA, module Data.Complex) where

import Data.Complex ( imagPart, magnitude, realPart, Complex(..) ) 
import Text.Printf

type PA = Complex Double 

showPA :: PA -> String
showPA pa 
    | imagPart pa < 0  = printf "(%.1f %.1f⋅i)" (realPart pa) (imagPart pa)
    | imagPart pa > 0  = printf "(%.1f + %.1f⋅i)" (realPart pa) (imagPart pa)
    | otherwise        = printf "%.1f" (realPart pa)

showPAMultiplicative :: PA -> String
showPAMultiplicative 1 = ""
showPAMultiplicative (-1) = "-"
showPAMultiplicative a = showPA a

squareModulus :: PA -> Double
squareModulus = (**2) . magnitude

addPA :: a -> a -> Complex a
addPA = (:+)