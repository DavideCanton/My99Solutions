{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Ex50 where

--(***) Huffman codes.
--
--We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:
--
--Example in Haskell:
--
-- Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

import           Control.Arrow
import			 Control.Monad
import           Data.List
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Ord

type FreqTuple = (Char, Int)
data HTree = Leaf Char | Branch HTree HTree
type HRes = [(Char, String)]

makeTree :: [FreqTuple] -> HTree
makeTree l = mktree $ sortBy (comparing fst) [(w, Leaf x) | (x, w) <- l]
    where mktree [(_, t)] = t
          mktree ((w1,t1):(w2,t2):xs) = mktree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) xs

treeToBin :: HTree -> HRes
treeToBin (Leaf f) = [(f, "")]
treeToBin (Branch t1 t2) = concatMap f $ zip "01" [t1, t2]
    where f (c, t) = map (second (c:)) (treeToBin t)

huffman :: [FreqTuple] -> HRes
huffman = sortBy (comparing fst) . treeToBin . makeTree

makeFreqTuples :: String -> [FreqTuple]
makeFreqTuples s = M.toAscList (createMap s)
    where createMap = foldl' (\m c -> M.insertWith (+) c 1 m) M.empty

encodeHuffman :: HRes -> String -> Maybe String
encodeHuffman ht s = liftM concat $ mapM (`lookup` ht) s