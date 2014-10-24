module Ex18 where

--    (**) Extract a slice from a list.
--
--    Given two indices, i and k, the slice is the list containing the elements between the
--    i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
--
--    Example in Haskell:
--
--    Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
--    "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice l a b = map snd . filter (\(i,_) -> i >= a && i <= b) . zip [1..] $ l

slice' :: [a] -> Int -> Int -> [a]
slice' l a b = sliceH l 1
    where sliceH [] _ = []
          sliceH (x:xs) i
            | i < a     = sliceH xs (i+1)
            | i > b     = []
            | otherwise = x : sliceH xs (i+1)
            
            
slice'' :: [a] -> Int -> Int -> [a]
slice'' l a b = map snd . filter ((>=a) . fst) . zip [1..b] $ l            