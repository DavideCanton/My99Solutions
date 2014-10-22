module Ex.Ex3 where

--(*) Find the K'th element of a list. The first element in the list is number 1.
--
--Example in Haskell:
--
--Prelude> elementAt [1,2,3] 2
--2
--Prelude> elementAt "haskell" 5
--'e'

elementAt :: [a] -> Int -> a
elementAt []     _ = error "Invalid index"
elementAt (x:xs) n
    | n < 1     = error "Invalid index"
    | n == 1    = x 
    | otherwise = elementAt xs (n-1)
