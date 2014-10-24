module Ex14 where

--(*) Duplicate the elements of a list.
--
--Example in Haskell:
--
-- dupli [1, 2, 3]
--[1,1,2,2,3,3]


dupli :: [a] -> [a]
dupli = foldr (\e a -> e:e:a) []

dupli' :: [a] -> [a]
dupli' = (>>= replicate 2)

dupli'' :: [a] -> [a]
dupli'' [] = []
dupli'' (x:xs) = x:x:dupli xs
