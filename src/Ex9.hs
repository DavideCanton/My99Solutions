module Ex.Ex9 where

--(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
--
--Example in Haskell:
--
--Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
--["aaaa","b","cc","aa","d","eeee"]

pack :: Eq a => [a] -> [[a]]
pack l@(x:_) =
    let (gr, els) = span (==x) l
     in gr:pack els
pack [] = []