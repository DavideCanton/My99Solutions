module Ex19 where

--(**) Rotate a list N places to the left.
--
--Hint: Use the predefined functions length and (++).
--
--Examples in Haskell:
--
--Main> rotate ['a','b','c','d','e','f','g','h'] 3
--"defghabc"
-- 
--Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
--"ghabcdef"

rotate :: [a] -> Int -> [a]
rotate l n = drop x l ++ take x l   
    where len = length l
          x = (len + n) `mod` len