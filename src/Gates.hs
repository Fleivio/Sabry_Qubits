module Gates(module Virtual.Value, ket1, ket0, ketMinus, ketPlus, adder, hGate, idGate) where

import Virtual.Value
import Virtual.Adaptor

boolToQV :: Bool -> QV Bool
boolToQV b = mkQV [(b, 1)]

ket1 :: QV Bool
ket1 = mkQV [(True, 1)]

ket0 :: QV Bool
ket0 = mkQV [(False, 1)]

ketMinus :: QV Bool 
ketMinus = normalize $ mkQV [(True, 1), (False, -1)]

ketPlus :: QV Bool
ketPlus = normalize $ mkQV [(True, 1), (False, 1)]

xGate :: Qop Bool Bool
xGate = mkQop 
    [((False, True), 1),
     ((True, False), 1)]

yGate :: Qop Bool Bool
yGate = mkQop 
    [((False, True), 0 :+ (-1)),
     ((True, False), 0 :+ 1)]

zGate :: Qop Bool Bool
zGate = mkQop 
    [((False, False), 1),
     ((True, True), -1)]

hGate :: Qop Bool Bool
hGate = mkQop 
    [((False, False), 1),
     ((False, True), 1),
     ((True, False), 1),
     ((True, True), -1) ]

vGate :: Qop Bool Bool
vGate = mkQop
    [((False, False), 1),
     ((True, True), 0 :+ 1)]

vtGate :: Qop Bool Bool
vtGate = mkQop 
    [((False, False), 1),
     ((True, True), 0 :+ (-1))]

idGate :: Qop Bool Bool
idGate = mkQop 
    [((True, True), 1),
     ((False, False), 1)]

cnot :: Qop (Bool, Bool) (Bool, Bool)
cnot = mkCQop id xGate

toffoli :: Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffoli = mkCQop (uncurry (&&)) xGate

toffoli' :: Basis a => Basis u => Virt ((Bool, Bool), Bool) a u -> IO()
toffoli' vtriple = 
    let b = virtFromV vtriple ad_triple3
        mb = virtFromV vtriple ad_triple23
        tm = virtFromV vtriple ad_triple12
        tb = virtFromV vtriple ad_triple13
        cv = mkCQop id vGate
        cvt = mkCQop id vtGate
    in do
        app1 hGate b
        app1 cv mb
        app1 cnot tm
        app1 cvt mb
        app1 cnot tm
        app1 cv tb
        app1 hGate b

adder :: QV Bool -> QV Bool -> QV Bool -> IO (QV Bool, QV Bool)
adder x y inc_c =
    let out_c = ket0
        vals = x &* y &* inc_c &* out_c
    in do 
        r <- mkQR vals
        let v = virtFromR r
            vxyo = virtFromV v ad_quad124
            vxy  = virtFromV v ad_quad12
            vyio = virtFromV v ad_quad234
            vyi  = virtFromV v ad_quad23
            vio  = virtFromV v ad_quad34
        toffoli' vxyo -- alternative equivalent to app1 toffoli
        app1 cnot vxy
        app1 toffoli vyio
        app1 cnot vyi
        app1 cnot vxy
        (sumR, carryOut) <- observeVV vio
        return (boolToQV sumR, boolToQV carryOut)