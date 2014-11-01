module Ex34 where

--(**) Calculate Euler's totient function phi(m).
--
--Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
--
--Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
--
--Example in Haskell:
--
-- totient 10
--4

import           Data.List
import           Data.Ratio
import           Ex33
import           Ex35

totient :: Integral a => a -> a
totient n = genericLength [ x | x <- [1..n-1], coprime n x ]


totient' :: Integral a => a -> a
totient' n = n * numerator r `div` denominator r
    where r = foldl' (*) 1 [ 1 - 1 % p | p <- nub $ primeFactors n ]
