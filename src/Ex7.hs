module Ex.Ex7 where

--(**) Flatten a nested list structure.
--
--Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
--
--Example in Haskell:
--
--We have to define a new data type, because lists in Haskell are homogeneous.
--
-- data NestedList a = Elem a | List [NestedList a]
--Main> flatten (Elem 5)
--[5]
--Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
--[1,2,3,4,5]
--Main> flatten (List [])
--[]

import NestedList

flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List l) = concatMap flatten l