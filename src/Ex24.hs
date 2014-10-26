module Ex24 where

--Lotto: Draw N different random numbers from the set 1..M.
--
--Example in Haskell:
--
--Prelude System.Random>diff_select 6 49
--Prelude System.Random>[23,1,17,33,21,37]

import Ex23

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n