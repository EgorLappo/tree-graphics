module Types (Tree(..), LabTree) where

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

type LabTree = Tree String
