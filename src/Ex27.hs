module Ex27 where

--Group the elements of a set into disjoint subsets.
--Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
--Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
--
--You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
--
--Example in Haskell:
--
--P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
--[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
--(altogether 1260 solutions)
--
--27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
--[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
--(altogether 756 solutions)

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination _ []     = []
combination n (x:xs) = as ++ bs
  where as = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
        bs = [ (ys,x:zs) | (ys,zs) <- combination n xs ]
    
    
group :: [Int] -> [a] -> [[[a]]]
group []     _ = [[]]
group (n:ns) l = [as:cs | (as,bs) <- combination n l, cs <- group ns bs]