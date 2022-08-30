{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module CombTrees (unlabeledTree) where

import Control.Monad
import Diagrams.Prelude

import Types


-- *** DRAWING UNLABELED TOPOLOGIES ***
unlabeledTree :: Renderable (Path V2 Double) b => Tree a -> QDiagram b V2 Double Any
unlabeledTree =  strokePath . makeUnlabeledTree (0 ^& 0)

makeUnlabeledTree :: P2 Double -- the location (in global coordinates) at which we start drawing
                  -> Tree a -- the tree node which we are drawing right now
                  -> Path V2 Double -- the "backbone" of our tree
-- for a leaf don't draw anything
makeUnlabeledTree pos (Leaf _) = toPath (emptyTrail `at` pos)
-- for an internal node, combine the following:
makeUnlabeledTree pos t@(Node _ l r) =
    toPath edges <> -- the two edges that are immediate descendents of the node,
    makeUnlabeledTree leftChildPos l <> -- the path for the left subtree, and
    makeUnlabeledTree rightChildPos r -- the path for the right subtree, 
  where
    (lOff, rOff) = (subtreeOffsets . subtreeLeafCounts) t
    leftChildPos  = pos .+^ lOff
    rightChildPos = pos .+^ rOff
    edges :: Located (Trail V2 Double)
    edges = fromOffsets [negated lOff, rOff] `at` leftChildPos

-- calculating offsets is simple: all edges are going to be at the 45 degree angle,
-- and the tree with n leaves is going to be 2n-2 units wide, with 2 units between leaves.
-- then, the length of the edges descending from the root is inversely proportional
-- to the size of the matching subtree, which is exactly what we encode here
subtreeOffsets :: (Int, Int) -> (V2 Double, V2 Double)
subtreeOffsets counts = ((-r) ^& (-r), l  ^& (-l))
    where (l, r) = counts & each %~ fromIntegral -- i.e. "fmap" of fromIntegral to tuple

subtreeLeafCounts :: Tree a -> (Int, Int)
subtreeLeafCounts (Node _ l r) = (treeLeafCount l, treeLeafCount r)
subtreeLeafCounts (Leaf _) = (0,0)

treeLeafCount :: Tree a -> Int
treeLeafCount (Leaf _) = 1
treeLeafCount (Node _ l r) = treeLeafCount l + treeLeafCount r

-- *** ADDING LABELS *** 
labeledTree :: Renderable (Path V2 Double) b =>
    (LabTree -> QDiagram b V2 Double Any) -> -- the formatter for the leaf noeds
    (LabTree -> QDiagram b V2 Double Any) -> -- the formatter for the internal nodes
    LabTree ->                               -- the tree that we are drawing
    QDiagram b V2 Double Any                 -- the resulting tree
-- labeledTree leafFmt nodeFmt t = unlabeledTree t `atop` position (treeLabels leafFmt nodeFmt t)
labeledTree leafFmt nodeFmt = liftM2 atop unlabeledTree (position . treeLabels leafFmt nodeFmt)

treeLabels :: Renderable (Path V2 Double) b =>
    (LabTree -> QDiagram b V2 Double Any) -> -- the formatter for the leaf noeds
    (LabTree -> QDiagram b V2 Double Any) -> -- the formatter for the internal nodes
    LabTree ->                               -- the tree that we are labeling
    [(P2 Double, QDiagram b V2 Double Any)]  -- the resulting positioned labels
treeLabels t leafFmt nodeFmt = undefined


