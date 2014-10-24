module Ex17 where

--(*) Split a list into two parts; the length of the first part is given.
--
--Do not use any predefined predicates.
--
--Example in Haskell:
--
--Main> split "abcdefghik" 3
--("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split [] _     = ([], [])
split l 0      = ([], l)
split (x:xs) n = let (h,t) = split xs (n-1)
                  in (x:h, t)
                                   