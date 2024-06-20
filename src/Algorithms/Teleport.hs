module Algorithms.Teleport
  ( _ex
  ) where

import Virtual.Adaptor
import Virtual.Value
import Gates

entangle :: (Basis a) => (Basis u) => Virt (Bool, Bool) a u -> IO ()
entangle v = do
    let vx = virtFromV v ad_pair1
    app1 h vx
    app1 cnot v

teleport :: (Basis a) => (Basis u) => Virt ((Bool, Bool), Bool) a u -> IO ()
teleport v = do
    let 
        v12 = virtFromV v ad_triple12
        v23 = virtFromV v ad_triple23
        v13 = virtFromV v ad_triple13
        v1  = virtFromV v ad_triple1
    entangle v23
    app1 cnot v12
    app1 h v1
    observeVV v12
    app1 cnot v23
    app1 cz v13

_ex :: IO()
_ex = do
  v <- virtFromList [(((True, False), False), 1), (((False, False), False), -1)]
  teleport v
  printVirt v


