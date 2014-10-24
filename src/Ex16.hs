module Ex16 where

--(**) Drop every N'th element from a list.
--
--Example in Haskell:
--
--Main> dropEvery "abcdefghik" 3
--"abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery l n = map snd . filter ((/=0) . (`mod` n) . fst) . zip [1..] $ l


dropEvery' :: [a] -> Int -> [a]
dropEvery' l n = dropEveryH' l n
    where dropEveryH' [] _     = []
          dropEveryH' (_:xs) 1 = dropEveryH' xs n
          dropEveryH' (x:xs) m = x:dropEveryH' xs (m-1)
          
dropEvery'' :: [a] -> Int -> [a]
dropEvery'' l n = map snd . filter ((/=n) . fst) . zip (cycle [1..n]) $ l