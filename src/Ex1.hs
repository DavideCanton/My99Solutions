module Ex1 where

--(*) Find the last element of a list.
--
--(Note that the Lisp transcription of this problem is incorrect.)
--
--Example in Haskell:
--
--Prelude> myLast [1,2,3,4]
--4
--Prelude> myLast ['x','y','z']
--'z'

myLast :: [a] -> a
myLast []     = error "Lista vuota"
myLast [x]    = x
myLast (_:xs) = myLast xs


myLast' :: [a] -> a
myLast' = foldr1 (flip const)


myLast'' :: [a] -> a
myLast'' = foldl1 (\_ e -> e)
