module Main(main) where

import Gates

main :: IO ()
main = do
        (a, b) <- adder ket1 ket1 ket0
        putStrLn $ "Sum = " <> showQV a
        putStrLn $ "Carry = " <> showQV b
