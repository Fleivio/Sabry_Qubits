module Algorithms.Teleport
  (teleport) where

import Virtual.Adaptor
import Virtual.Value
import Gates

entangle :: (Basis a) => (Basis u) => Virt (Bool, Bool) a u -> IO ()
entangle v = do
    let vx = virtFromV v ad_pair1
    app1 h vx
    app1 cnot v

teleport :: (Basis a) => (Basis u) => (Show u) => Virt ((Bool, Bool), Bool) a u -> IO ()
teleport v = do
    let
        v12 = virtFromV v ad_triple12
        v23 = virtFromV v ad_triple23
        v13 = virtFromV v ad_triple13
        v1  = virtFromV v ad_triple1
    putStrLn "\nValor de entrada:"
    printVirt v

    entangle v23
    putStrLn "\nEmaranhamento 2-3"
    printVirt v

    app1 cnot v12
    putStrLn "\nCNOT 1-2"
    printVirt v

    app1 h v1
    putStrLn "\nH 1"
    printVirt v

    _ <- observeVV v12
    putStrLn "\nMedição 1-2"
    printVirt v

    app1 cnot v23
    putStrLn "\nCNOT 2-3"
    printVirt v

    app1 cz v13
    putStrLn "\nCZ 1-3"
    printVirt v



