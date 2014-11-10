module Ex49 where

--(**) Gray codes.
--
--An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
--
--n = 1: C(1) = ['0','1'].
--n = 2: C(2) = ['00','01','11','10'].
--n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
--
--
--Example in Haskell:
--
--P49> gray 3
--["000","001","011","010","110","111","101","100"]

gray :: Int -> [String]
gray 0 = []
gray 1 = ["0", "1"]
gray n = map ('0':) gray1 ++ map ('1':) (reverse gray1) 
    where gray1 = gray (n-1)   