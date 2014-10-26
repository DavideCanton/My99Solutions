module Ex25 where

--Generate a random permutation of the elements of a list.
--
--Example in Haskell:
--
--Prelude System.Random>rnd_permu "abcdef"
--Prelude System.Random>"badcef"

import           Ex23
import           System.Random

rndPermu :: [a] -> IO [a]
rndPermu l = rndSelect l (length l)

rndPermu' :: [a] -> IO [a]
rndPermu' [] = return []
rndPermu' (x:xs) = do
    i <- randomRIO (0, length xs)
    res <- rndPermu' xs
    let (ys, zs) = splitAt i res
    return $ ys ++ (x:zs)
