module Ex31 where

--(**) Determine whether a given integer number is prime.
--
--Example in Haskell:
--
--P31> isPrime 7
--True

isPrime :: Integral a => a -> Bool
isPrime n
    | n < 2          = False
    | n == 2         = True 
    | n `mod` 2 == 0 = False
    | otherwise      = all (\x -> n `mod` x /= 0) [3, 5..limit]
    where limit = floor . (sqrt :: Double -> Double) . fromIntegral $ n