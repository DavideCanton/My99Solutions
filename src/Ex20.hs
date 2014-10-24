module Ex20 where

--(*) Remove the K'th element from a list.
--
--Example in Haskell:
--
--Main> removeAt 2 "abcd"
--('b',"acd")

import           Control.Arrow


removeAt :: Int -> [a] -> (a, [a])
removeAt _ []     = error "Invalid list or index"
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = second (x:) $ removeAt (n - 1) xs
