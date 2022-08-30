{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module CombTrees (unlabeledTree) where

import Types

import Diagrams.Prelude

-- draw unlabeled topology
-- for leaves return a point 
-- for nodes compute points of sister nodes, draw lines to them
-- then draw subtrees starting with those computed points

unlabeledTree :: Renderable (Path V2 Double) b => Tree a -> QDiagram b V2 Double Any
unlabeledTree =  strokePath . makeUnlabeledTree (0 ^& 0)

makeUnlabeledTree :: P2 Double -> Tree a -> Path V2 Double --Path V2 Double
makeUnlabeledTree pos (Leaf _) = toPath (emptyTrail `at` pos)-- what do i put to get an empty diagram?
makeUnlabeledTree pos t@(Node _ l r) = 
    toPath edges <>
    makeUnlabeledTree leftChildPos l <>
    makeUnlabeledTree rightChildPos r
  where 
    (lOff, rOff) = (subtreeOffsets . subtreeLeafCounts) t
    leftChildPos  = pos .+^ lOff
    rightChildPos = pos .+^ rOff
    edges :: Located (Trail V2 Double)
    edges = fromOffsets [negated lOff, rOff] `at` leftChildPos

subtreeOffsets :: (Int, Int) -> (V2 Double, V2 Double)
subtreeOffsets counts = ((-r) ^& (-r), (-l)  ^& (-l))
    where (l, r) = counts & each %~ fromIntegral -- i.e. "fmap" of fromIntegral to tuple

subtreeLeafCounts :: Tree a -> (Int, Int)
subtreeLeafCounts (Node _ l r) = (treeLeafCount l, treeLeafCount r)
subtreeLeafCounts (Leaf _) = (0,0)

treeLeafCount :: Tree a -> Int
treeLeafCount (Leaf _) = 1
treeLeafCount (Node _ l r) = treeLeafCount l + treeLeafCount r



