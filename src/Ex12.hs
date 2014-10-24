module Ex12 where

--(**) Decode a run-length encoded list.
--
--Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--
--Example in Haskell:
--
--P12> decodeModified 
--       [Multiple 4 'a',Single 'b',Multiple 2 'c',
--        Multiple 2 'a',Single 'd',Multiple 4 'e']
--"aaaabccaadeeee"

import Encoded

decodeModified :: [Encoded a] -> [a]
decodeModified = (>>=fun)
    where fun (Single e) = [e]
          fun (Multiple n e) = replicate n e