module Algorithms.Deutsch(deutsch) where

import Virtual.Adaptor
import Virtual.Value
import Gates

deutsch :: (Basis a, Basis u, Show u) => Virt (Bool, Bool) a u -> (Bool -> Bool) -> IO Bool
deutsch both f = do
  let top = virtFromV both ad_pair1
      bot = virtFromV both ad_pair2
      uf = mkCQop f x

  app1 h top
  app1 h bot

  printVirt both
  
  app1 uf both
  
  printVirt both

  app1 h top

  printVirt both

  observeVV top

