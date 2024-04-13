module Quantum.PA (PA, showPA, showPAMultiplicative, squareModulus, module Data.Complex) where

import Data.Complex ( imagPart, magnitude, realPart, Complex(..) ) 
import Text.Printf

type PA = Complex Double 

showPA :: PA -> String
showPA pa 
    | imagPart pa < 0  = printf "(%.1f %.1f⋅i)" (realPart pa) (imagPart pa)
    | imagPart pa > 0  = printf "(%.1f + %.1f⋅i)" (realPart pa) (imagPart pa)
    | otherwise        = printf "%.1f" (realPart pa)

showPAMultiplicative :: PA -> String
showPAMultiplicative pa = 
    case pa of
        1    -> mempty
        (-1) -> "-"
        a    ->  showPA a

squareModulus :: PA -> Double
squareModulus = (**2) . magnitude