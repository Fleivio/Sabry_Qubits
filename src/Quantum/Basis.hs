module Quantum.Basis(Basis, basis) where

class Eq a => Basis a where
    basis :: [a]

instance Basis Bool where
    basis = [False, True]

