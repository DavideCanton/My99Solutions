module Main where

import Ex11

main :: IO ()
main = do
    let l = "aaaabccaadeeee"
    print l
    print $ encodeModified l