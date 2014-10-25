module Ex21 where

--Insert an element at a given position into a list.
--
--Example in Haskell:
--
--P21> insertAt 'X' "abcd" 2
--"aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt el l 1 = el:l
insertAt el (x:xs) n = x:insertAt el xs (n-1)
insertAt _  [] _ = error "Invalid index"


insertAt' :: a -> [a] -> Int -> [a]
insertAt' el l n = foldr step [] . zip [1..] $ l
    where step (i, e) acc
            | i == n    = el:e:acc
            | otherwise = e:acc