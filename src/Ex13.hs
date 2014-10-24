module Ex13 where

--(**) Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9,
-- but only count them. As in problem P11, simplify the result list by replacing the singleton
-- lists (1 X) by X.
--
--Example in Haskell:
--
--P13> encodeDirect "aaaabccaadeeee"
--[Multiple 4 'a',Single 'b',Multiple 2 'c',
-- Multiple 2 'a',Single 'd',Multiple 4 'e']

import           Encoded

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect l = map fun $ foldr step [] l
    where fun (1,x) = Single x
          fun (n,x) = Multiple n x
          step x [] = [(1, x)]
          step x y@((n, h) : ys)
            | x == h    = (n + 1,h) : ys
            | otherwise = (1,x) : y
