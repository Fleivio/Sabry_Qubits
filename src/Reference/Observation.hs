{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Reference.Observation (observeV, observeR, observeLeftR, observeRightR,
    module Reference.Reference) where

import Reference.Reference

import Data.IORef
import Data.List (find)
import System.Random (getStdRandom, Random(randomR))

observeV :: Basis a => QV a -> IO a
observeV v = do
    let nv = normalize v
        probs = squareModulus . getProb nv <$> basis
    r <- getStdRandom $ randomR (0.0, 1.0)
    let accumulatedProbs = zip (scanl1 (+) probs) basis
        -- never yields Nothing due to normalization
        Just (_, res) = find ((r<) . fst) accumulatedProbs
    return res


observeR :: Basis a => QR a -> IO a
observeR (QR ptr) = do
    qVal <- readIORef ptr
    observResult <- observeV qVal
    writeIORef ptr (mkQV [(observResult, 1)])
    return observResult

observeLeftR :: (Basis a, Basis b) => QR (a, b) -> IO a
observeLeftR (QR ptr) = do
    qVal <- readIORef ptr
    let leftProb a = sqrt . sum $ [ squareModulus (getProb qVal (a,b)) :+ 0 | b <- basis]
        leftQval = mkQV [(a, leftProb a) | a <- basis]
    observResult <- observeV leftQval
    let nv = mkQV [((observResult, b), getProb qVal (observResult, b)) | b <- basis]
    writeIORef ptr (normalize nv)
    return observResult

observeRightR :: (Basis a, Basis b) => QR (a, b) -> IO b
observeRightR (QR ptr) = do
    qVal <- readIORef ptr
    let rightProb a = sqrt . sum $ [ squareModulus (getProb qVal (b,a)) :+ 0 | b <- basis]
        rightQval = mkQV [(a, rightProb a) | a <- basis]
    observResult <- observeV rightQval
    let nv = mkQV [((b, observResult), getProb qVal (b, observResult)) | b <- basis]
    writeIORef ptr (normalize nv)
    return observResult