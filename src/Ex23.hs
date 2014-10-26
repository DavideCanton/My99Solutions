module Ex23 where

--Extract a given number of randomly selected elements from a list.
--
--Example in Haskell:
--
--Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
--eda

import           Ex20
import           System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0          = return []
rndSelect [] n | n > 0 = error "Invalid index"
rndSelect l n          = do
    i <- randomRIO (0, length l - 1)
    let removed = snd $ removeAt (i + 1) l
    xs <- rndSelect removed (n - 1)
    return $ (l !! i):xs