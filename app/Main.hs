module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import CombTrees
import Types

main :: IO ()
main = mainWith $ (unlabeledTree tree1 :: Diagram B)

tree1 :: LabTree
tree1 = Node "c" (Leaf "a") (Leaf "b")

tree2 :: LabTree
tree2 = Node "e" (Node "d" (Leaf "a") (Leaf "b")) (Leaf "c")

tree3 :: LabTree
tree3 = Node "g" (Node "e" (Leaf "a") (Leaf "b")) (Node "f" (Leaf "c") (Leaf "d"))