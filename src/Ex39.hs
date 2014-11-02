module Ex39 where

--(*) A list of prime numbers.
--
--Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
--
--Example in Haskell:
--
--P29> primesR 10 20
--[11,13,17,19]

import Ex31

primesR :: Integral a => a -> a -> [a]
primesR a b = filter isPrime [a..b]


primesR' :: Integral a => a -> a -> [a]
primesR' a b = filter (>=a) $ sieve [2..b]
    where sieve l@(x:xs)            
            | x * x >= b = l
            | otherwise  = x : sieve (filter (\e -> e `mod` x /= 0) xs)
          sieve [] = []