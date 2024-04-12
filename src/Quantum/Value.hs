{-#LANGUAGE LambdaCase #-}
module Quantum.Value(QV, getProb, (&*), mkQV, showQV) where

import Quantum.PA
import Quantum.Basis

import Data.Map as Map
import Data.List

type QV a = Map a PA

getProb :: Basis a => QV a -> a -> PA
getProb qmap index = Map.findWithDefault 0 index qmap

(&*) :: Basis a => Basis b => QV a -> QV b -> QV (a, b)
qmap1 &* qmap2 = 
    mkQV
    [((x, y), getProb qmap1 x * getProb qmap2 y) | (x, y) <- basis]

mkQV :: Basis a => [(a, PA)] -> QV a
mkQV = Map.fromList . Prelude.filter ((/= 0).snd)

showQV :: Show a => QV a -> String
showQV qv = intercalate " + " $ do
    (a, pa) <- toList qv
    return 
        $ case pa of 
            0 -> mempty
            _ -> showPAMultiplicative pa ++ "|" ++ show a ++ "⟩" 