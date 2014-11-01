module Ex32 where

--(**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--
--Example in Haskell:
--
--[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
--[9,3,3]

myGCD :: Int -> Int -> Int
myGCD n1 n2 = myGCD' (abs n1) (abs n2)
    where myGCD' a 0 = a
          myGCD' a b = myGCD' b (a `mod` b)