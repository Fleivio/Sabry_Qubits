module Reference.Reference (
    mkQR, QR(..), qrApp, printQR, showQR,
    module Quantum.Operators) where

import Quantum.Operators
import Data.IORef

data QR a = QR (IORef (QV a))

mkQR :: QV a -> IO (QR a)
mkQR qv = QR <$> newIORef qv

qrApp :: Basis a => Qop a a -> QR a -> IO ()
qrApp op (QR ref) = modifyIORef ref (appQop op)

showQR :: Show a => QR a -> IO String 
showQR (QR ref) = do
    qval <- readIORef ref 
    return $ showQV qval

printQR :: (Show a) => QR a -> IO()
printQR (QR ref) = do
    qval <- readIORef ref 
    putStrLn $ showQV qval