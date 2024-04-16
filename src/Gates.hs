module Gates
  ( module Virtual.Value
  , ket1
  , ket0
  , ketMinus
  , ketPlus
  , adder
  , hGate
  , idGate
  , yGate
  , zGate
  , vGate
  , vtGate
  ) where

import Virtual.Adaptor
import Virtual.Value

ket1 :: QV Bool
ket1 = mkQV [(True, 1)]

ket0 :: QV Bool
ket0 = mkQV [(False, 1)]

ketMinus :: QV Bool
ketMinus = normalize $ mkQV [(True, 1), (False, -1)]

ketPlus :: QV Bool
ketPlus = normalize $ mkQV [(True, 1), (False, 1)]

xGate :: Qop Bool Bool
xGate = mkQop [((False, True), 1), ((True, False), 1)]

yGate :: Qop Bool Bool
yGate = mkQop [((False, True), 0 :+ (-1)), ((True, False), 0 :+ 1)]

zGate :: Qop Bool Bool
zGate = mkQop [((False, False), 1), ((True, True), -1)]

hGate :: Qop Bool Bool
hGate =
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
cnot = mkCQop id xGate

toffoli :: Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffoli = mkCQop (uncurry (&&)) xGate

adder ::
     (Basis a) => (Basis u) => Virt (((Bool, Bool), Bool), Bool) a u -> IO ()
adder v = do
  let vxyo = virtFromV v ad_quad124
      vxy = virtFromV v ad_quad12
      vyio = virtFromV v ad_quad234
      vyi = virtFromV v ad_quad23
  app1 toffoli vxyo
  app1 cnot vxy
  app1 toffoli vyio
  app1 cnot vyi
  app1 cnot vxy
