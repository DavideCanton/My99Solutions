module Ex11 where

--(*) Modified run-length encoding.
--
--Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
--Example in Haskell:
--
--P11> encodeModified "aaaabccaadeeee"
--[Multiple 4 'a',Single 'b',Multiple 2 'c',
-- Multiple 2 'a',Single 'd',Multiple 4 'e']

import Encoded
import Ex10

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map func . encode
    where func (1, x) = Single x
          func (n, x) = Multiple n x