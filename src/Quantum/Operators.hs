module Quantum.Operators
  ( mkQop
  , appQop
  , mkCQop
  , Qop(..)
  , module Quantum.Value
  ) where

import Data.Map
import Quantum.Value

newtype Qop a b =
  Qop (Map (a, b) PA)

mkQop :: (Basis a, Basis b) => [((a, b), PA)] -> Qop a b
mkQop = Qop . fromList

mkCQop :: (Basis a, Basis b) => (a -> Bool) -> Qop b b -> Qop (a, b) (a, b)
mkCQop enable (Qop qop) = mkQop $ unchangeCase ++ changeCase
        -- enable = false
  where
    unchangeCase = [(((a, b), (a, b)), 1) | (a, b) <- basis, not (enable a)]
        -- enable = true
    changeCase =
      [ (((a, b1), (a, b2)), getProb qop (b1, b2))
      | a <- basis
      , enable a
      , b1 <- basis
      , b2 <- basis
      ]

appQop :: (Basis a, Basis b) => Qop a b -> QV a -> QV b
appQop (Qop qop) qv = mkQV [(b, prob b) | b <- basis]
  where
    prob b = sum [qop `getProb` (a, b) * qv `getProb` a | a <- basis]
