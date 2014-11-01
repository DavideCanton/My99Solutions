module Ex22 where

--Create a list containing all integers within a given range.
--
--Example in Haskell:
--
--Prelude> range 4 9
--[4,5,6,7,8,9]

import           Data.List

range :: Integral a => a -> a -> [a]
range a b
    | a == b    = [a]
    | a < b     = a:range (a+1) b
    | otherwise = []


range' :: Integral a => a -> a -> [a]
range' a b = unfoldr step a
    where step n
            | n > b     = Nothing
            | otherwise = Just (n, n + 1)


range'' :: Integral a => a-> a -> [a]
range'' a b = take (fromIntegral $ b - a + 1) . iterate (+1) $ a


range''' :: Integral a => a -> a -> [a]
range''' a b = init $ scanl (+) a (replicate (fromIntegral $ b - a + 1) 1)
