module Ex36 where

--(**) Determine the prime factors of a given positive integer.
--
--Construct a list containing the prime factors and their multiplicity.
--
--Example in Haskell:
--
--Main> prime_factors_mult 315
--[(3,2),(5,1),(7,1)]

import           Control.Arrow
import           Data.List
import           Ex35

primeFactorsMult :: (Integral a, Integral b) => a -> [(b, Int)]
primeFactorsMult = map (fromIntegral . head &&& length) . group . primeFactors
