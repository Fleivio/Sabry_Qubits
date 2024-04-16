module Reference.Reference
  ( mkQR
  , QR(..)
  , qrApp
  , printQR
  , showQR
  , module Quantum.Operators
  , qrFromList
  ) where

import Data.IORef
import Quantum.Operators

newtype QR a =
  QR (IORef (QV a))

mkQR :: QV a -> IO (QR a)
mkQR qv = QR <$> newIORef qv

qrFromList :: Basis a => [(a, PA)] -> IO (QR a)
qrFromList lst = mkQR $ mkQV lst

qrApp :: Basis a => Qop a a -> QR a -> IO ()
qrApp op (QR ref) = modifyIORef ref (appQop op)

showQR :: Show a => QR a -> IO String
showQR (QR ref) = do
  qval <- readIORef ref
  return $ showQV qval

printQR :: (Show a) => QR a -> IO ()
printQR (QR ref) = do
  qval <- readIORef ref
  putStrLn $ showQV qval
