module Ex33 where

--(*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--
--Example in Haskell:
--
--coprime 35 64
--True


coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1