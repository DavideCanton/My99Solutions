module Ex41 where

--(**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their 
--Goldbach composition.
--
--In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. 
--Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
--
--Example in Haskell:
--
--Exercises> goldbachList 9 20
--[(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
--Exercises> goldbachList' 4 2000 50
--[(73,919),(61,1321),(67,1789),(61,1867)]

import Ex40

goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList a b = map goldbach . filter even $ [a..b]

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' a b n = [ (x, y) | (x, y) <- goldbachList a b, x > n, y > n ]