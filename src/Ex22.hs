module Ex22 where

--Create a list containing all integers within a given range.
--
--Example in Haskell:
--
--Prelude> range 4 9
--[4,5,6,7,8,9]

import           Data.List

range :: Int -> Int -> [Int]
range a b
    | a == b    = [a]
    | a < b     = a:range (a+1) b
    | otherwise = []


range' :: Int -> Int -> [Int]
range' a b = unfoldr step a
    where step n
            | n > b     = Nothing
            | otherwise = Just (n, n + 1)


range'' :: Int -> Int -> [Int]
range'' a b = take (b - a + 1) . iterate (+1) $ a


range''' :: Int -> Int -> [Int]
range''' a b = init $ scanl (+) a (replicate (b - a + 1) 1)
