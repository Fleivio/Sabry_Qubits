module Algorithms.Deutsch(_ex) where

import Virtual.Adaptor
import Virtual.Value
import Gates

deutsch :: (Bool -> Bool) -> IO Bool
deutsch f = do
    v <- virtFromList [((False,True), 1)]
    let 
        top = virtFromV v ad_pair1
        bot = virtFromV v ad_pair2
        qf  = mkCQop f x
    app1 h top
    app1 h bot
    app1 qf v
    app1 h top
    observeVV top

_ex :: IO ()
_ex = do
    b <- deutsch id
    print $ 
        "id is "
        <> if b
            then "Balanced"
            else "Constant"
    
    b1 <- deutsch not
    print $ 
        "not is "
        <> if b1
            then "Balanced"
            else "Constant"

    b2 <- deutsch (const False)
    print $ 
        "(const False) is "
        <> if b2
            then "Balanced"
            else "Constant"