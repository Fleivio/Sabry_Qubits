module Gates() where

import Virtual.Value

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

adder :: QV Bool -> QV Bool -> QV Bool -> IO (QV Bool, QV Bool)
adder inc x y =
    let outc = bra_0_ket
        vals = x &* y &* inc &* outc
    in do 
        r <- mkQR vals
        let v = virtFromR r
            vxyo = virtFromV v ad_quad124
            vxy = virtFromV v ad_quad12
            vyio = virtFromV v ad_quad234
            vyi = virtFromV v ad_quad23
            vio = virtFromV v ad_quad34
        app1 toffoli vxyo
        app1 cnot vxy
        app1 toffoli vyio
        app1 cnot vyi
        app1 cnot vxy
        (sumR, carryOut) <- observeVV vio
        putStrLn $ qvToString x ++ " +. " ++ qvToString y ++ " +. " ++ qvToString inc
        putStrLn $ "Sum = " ++ show sumR ++ "\nCarry = " ++ show carryOut
        return (boolToQv carryOut, boolToQv sumR)