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
import           Ex33

totient :: (Integral a, Integral b) => a -> b
totient n = genericLength [ x | x <- [1..n-1], coprime n x ]