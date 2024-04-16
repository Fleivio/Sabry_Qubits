{-# LANGUAGE LambdaCase #-}
module Algorithms.BB84(bb84) where

import Gates

import System.Random
import Control.Concurrent
import Control.Monad

data ObservationBasis = Z | X deriving (Show, Eq)

type SQuBit = Virt Bool () Bool
type QuBitChan = Chan SQuBit

observeAtBasis :: Basis a => Basis b =>
    ObservationBasis -> Virt Bool a b -> IO Bool
observeAtBasis Z val = observeVV val
observeAtBasis X val = do
    app1 h val
    observation <- observeVV val
    app1 h val
    return observation

--------------------------------------------------

type ObsBasis = [ObservationBasis]

data EndPoint = EndPoint ObsBasis [Bool]

instance Show EndPoint where
    show (EndPoint o m) = show $ zip o m

randomObsBasis :: Int -> IO ObsBasis
randomObsBasis n = do
    ix <- take n . randomRs (1,0) <$> newStdGen
    return $ ([Z, X]!!) <$> ix

randomBits :: Int -> IO [Bool]
randomBits n = take n . randomRs (True, False) <$> newStdGen

prepareQuBit :: Bool -> ObservationBasis -> IO SQuBit
prepareQuBit False Z = virtFromR <$> mkQR ket0
prepareQuBit True  Z = virtFromR <$> mkQR ket1
prepareQuBit False X = virtFromR <$> mkQR ketPlus
prepareQuBit True  X = virtFromR <$> mkQR ketMinus

randomQubits :: Int -> IO EndPoint
randomQubits n = do
    obsB <- randomObsBasis n
    values <- randomBits n
    return $ EndPoint obsB values

endPoints :: Int -> IO (EndPoint, EndPoint, EndPoint)
endPoints size = do
    alice <- randomQubits size
    trudy <- EndPoint <$> randomObsBasis size <*> pure []
    bob   <- EndPoint <$> randomObsBasis size <*> pure []
    return (alice, bob, trudy)

populateChan :: EndPoint -> QuBitChan -> IO ()
populateChan (EndPoint o m) chan =
    writeList2Chan chan =<< zipWithM prepareQuBit m o

interceptBetweenChans :: EndPoint -> QuBitChan -> QuBitChan -> IO EndPoint
interceptBetweenChans (EndPoint obsB _) chan1 chan2 = do
    measure <- run obsB
    return $ EndPoint obsB measure
    where
        run :: ObsBasis -> IO [Bool]
        run []     = return []
        run (b:bs) = do
                q' <- readChan chan1
                m <- observeAtBasis b q'
                writeChan chan2 q'
                (m:) <$> run bs

receiveFromChan :: EndPoint -> QuBitChan -> IO EndPoint
receiveFromChan (EndPoint obsB _) chan1 = do
    measure <- run obsB
    return $ EndPoint obsB measure
    where
        run :: ObsBasis -> IO [Bool]
        run [] = return []
        run (b:bs) = do
            q' <- readChan chan1
            m <- observeAtBasis b q'
            (m:) <$> run bs

bb84run :: Int -> IO (EndPoint, EndPoint, EndPoint)
bb84run size = do

    (alice, bob, trudy) <- endPoints size

    chan1 <- newChan
    chan2 <- newChan

    bob'   <- newEmptyMVar
    trudy' <- newEmptyMVar

    _ <- forkIO (populateChan alice chan1)
    _ <- forkFinally (interceptBetweenChans trudy chan1 chan2)
                (\case
                    Left _ -> putMVar trudy' trudy
                    Right a -> putMVar trudy' a)
    _ <- forkFinally (receiveFromChan bob chan2)
                (\case
                    Left _ -> putMVar bob' bob
                    Right a -> putMVar bob' a)

    bob''   <- takeMVar bob'
    trudy'' <- takeMVar trudy'

    return (alice, trudy'', bob'')

getKey :: (EndPoint, EndPoint) -> Either [Bool] [Bool]
getKey (EndPoint aliceBasis aliceBits, EndPoint bobBasis bobBits) =
    let matches = zipWith (==) aliceBasis bobBasis
        aliceMatches = [bit | (bit, m) <- zip aliceBits matches, m]
        bobMatches   = [bit | (bit, m) <- zip bobBits matches, m]
        matchesMask  = zipWith (==) aliceMatches bobMatches
    in if and matchesMask
        then Left aliceMatches
        else Right matchesMask

bb84 :: Int -> IO ()
bb84 size = do
    (alice, trudy, bob) <- bb84run size
    print alice
    print trudy
    print bob
    case getKey (alice, bob) of
        Right err  -> do
            print "Error: Alice and Bob have different bits!"
            print $ "Matching Failed: " <> show err
        Left key -> do
            print "Success: Alice and Bob have the same bits!"
            print key