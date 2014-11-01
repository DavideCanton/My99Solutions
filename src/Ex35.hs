module Ex35 where

--(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
--
--Example in Haskell:
--
-- primeFactors 315
--[3, 3, 5, 7]

primeFactors :: Integral a => a -> [a]
primeFactors k = primeFactorsH k 2
    where primeFactorsH n m
            | n < m          = []
            | n `mod` m == 0 = m:primeFactorsH (n `div` m) m
            | otherwise      = primeFactorsH n (if m == 2 then 3 else m + 2)