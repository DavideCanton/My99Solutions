module Ex.NestedList where

data NestedList a = Elem a | List [NestedList a] deriving Eq

instance Show a => Show (NestedList a) where
    show (Elem e) = show e
    show (List l) = show l
