module Ex.Ex6 where

--(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
--
--Example in Haskell:
--
--Main> isPalindrome [1,2,3]
--False
--Main> isPalindrome "madamimadam"
--True
--Main> isPalindrome [1,2,4,8,16,8,4,2,1]
--True

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       ((&&&))

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l


isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' = (==) <$> id <*> reverse


isPalindrome'' :: Eq a => [a] -> Bool
isPalindrome'' []  = True
isPalindrome'' [_] = True
isPalindrome'' l   = head l == last l && isPalindrome'' (init . tail $ l)


isPalindrome''' :: Eq a => [a] -> Bool
isPalindrome''' l = foldl step True [0..n `div` 2]
    where n = length l
          step False _ = False
          step _     i = l !! i == l !! (n - 1 - i)


isPalindrome'''' :: Eq a => [a] -> Bool
isPalindrome'''' = uncurry (==) . (id &&& reverse)
