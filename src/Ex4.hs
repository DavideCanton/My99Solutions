module Ex4 where

--(*) Find the number of elements of a list.
--
--Example in Haskell:
--
--Prelude> myLength [123, 456, 789]
--3
--Prelude> myLength "Hello, world!"
--13

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs


myLength' :: [a] -> Int
myLength' = sum . map (const 1)


myLength'' :: [a] -> Int
myLength'' = foldl (\a _ -> a + 1) 0