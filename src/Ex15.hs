module Ex15 where

--(**) Replicate the elements of a list a given number of times.
--
--Example in Haskell:
--
-- repli "abc" 3
--"aaabbbccc"

repli :: [a] -> Int -> [a]
repli l n = l >>= replicate n

repli' :: [a] -> Int -> [a]
repli' [] _     = []
repli' (x:xs) n = replicate n x ++ repli' xs n

repli'' :: [a] -> Int -> [a]
repli'' l n = concatMap (myRepl n) l
    where myRepl 0 _ = []
          myRepl m e = e:myRepl (m - 1) e