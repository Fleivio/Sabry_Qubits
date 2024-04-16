module Algorithms.Adder(_ex) where

import Virtual.Adaptor
import Virtual.Value
import Gates

adder :: (Basis a) => (Basis u) => Virt (((Bool, Bool), Bool), Bool) a u -> IO ()
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

_ex :: IO ()
_ex = do
  v <- virtFromList [((((True, True), True), False), 1)]
  adder v
  (((a, b), c), d) <- observeVV v
  putStrLn $ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
