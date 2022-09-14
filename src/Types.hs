module Types (Tree(..), LabTree) where

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

type LabTree = Tree String

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)


