module Ex.Ex2 where

--(*) Find the last but one element of a list.
--
--(Note that the Lisp transcription of this problem is incorrect.)
--
--Example in Haskell:
--
--Prelude> myButLast [1,2,3,4]
--3
--Prelude> myButLast ['a'..'z']
--'y'

myButLast :: [a] -> a
myButLast []     = error "Invalid list"
myButLast [_]    = error "Invalid list"
myButLast [a,_]  = a
myButLast (_:xs) = myButLast xs