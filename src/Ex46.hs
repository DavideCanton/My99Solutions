module Ex46 where

--(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
--
--A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
--
--Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

--Example in Haskell:
--
-- table (\a b -> (and' a (or' a b)))
--True True True
--True False True
--False True False
--False False False

import Control.Monad

type Bool2Fun = Bool -> Bool -> Bool

infixl 4 `or'`
infixl 6 `and'`

and' :: Bool2Fun
and' = (&&)

or' :: Bool2Fun
or' = (||)


nand' :: Bool2Fun
nand' a b = not $ and' a b


nor' :: Bool2Fun
nor' a b = not $ or' a b


xor' :: Bool2Fun
xor' = (/=)


impl' :: Bool2Fun
impl' a b = not a || b


equ' :: Bool2Fun
equ' = (==)

table :: Bool2Fun -> IO ()
table f = do
    let values = liftM2 (,) [True, False] [True, False]
    forM_ values $ \(a,b) -> putStrLn $ show a ++ " " ++ show b ++ " " ++ show (f a b)