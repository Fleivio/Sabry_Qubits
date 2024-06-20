module Gates
  ( module Virtual.Value
  , ket1
  , ket0
  , ketMinus
  , ketPlus
  , h
  , idGate
  , y
  , z
  , vGate
  , vtGate
  , x
  , cnot
  , toffoli
  , cz
  ) where

import Virtual.Value

ket1 :: QV Bool
ket1 = mkQV [(True, 1)]

ket0 :: QV Bool
ket0 = mkQV [(False, 1)]

ketMinus :: QV Bool
ketMinus = normalize $ mkQV [(True, -1), (False, 1)]

ketPlus :: QV Bool
ketPlus = normalize $ mkQV [(True, 1), (False, 1)]

x :: Qop Bool Bool
x = mkQop [((False, True), 1), ((True, False), 1)]

y :: Qop Bool Bool
y = mkQop [((False, True), 0 :+ (-1)), ((True, False), 0 :+ 1)]

z :: Qop Bool Bool
z = mkQop [((False, False), 1), ((True, True), -1)]

h :: Qop Bool Bool
h =
  mkQop
    [ ((False, False), 1)
    , ((False, True), 1)
    , ((True, False), 1)
    , ((True, True), -1)
    ]

vGate :: Qop Bool Bool
vGate = mkQop [((False, False), 1), ((True, True), 0 :+ 1)]

vtGate :: Qop Bool Bool
vtGate = mkQop [((False, False), 1), ((True, True), 0 :+ (-1))]

idGate :: Qop Bool Bool
idGate = mkQop [((True, True), 1), ((False, False), 1)]

cnot :: Qop (Bool, Bool) (Bool, Bool)
cnot = mkCQop id x

toffoli :: Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffoli = mkCQop (uncurry (&&)) x

cz :: Qop (Bool, Bool) (Bool, Bool)
cz = mkCQop id z