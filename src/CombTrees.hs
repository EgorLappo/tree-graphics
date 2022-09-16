module CombTrees (unlabeledTree, labeledTree, leafLabeledTree, debugTree) where

import Control.Monad ( liftM2 )
import Diagrams.Prelude

import Types ( LabTree, Tree(..) )


-- *** DRAWING UNLABELED TOPOLOGIES ***
unlabeledTree :: _ => Tree a -> QDiagram b V2 Double Any
unlabeledTree = lineCap LineCapRound
              . lw 5
              . strokePath
              . makeUnlabeledTree (0 ^& 0)

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
leafLabeledTree :: _ => LabTree ->  QDiagram b V2 Double Any
leafLabeledTree = frameTree . labeledTree' leafLabel empty

labeledTree :: _ => LabTree -> QDiagram b V2 Double Any
labeledTree = frameTree . labeledTree' leafLabel nodeLabel

debugTree :: _ => LabTree ->  QDiagram b V2 Double Any
debugTree = frameTree . labeledTree' (const pt) (\_ _ -> pt)

labeledTree' :: _ => (String ->  QDiagram b V2 Double Any)          -- the formatter for the leaf noeds
             -> (Nudge -> String ->  QDiagram b V2 Double Any) -- the formatter for the internal nodes
             -> LabTree                        -- the tree that we are drawing
             -> QDiagram b V2 Double Any                     -- the resulting tree
-- labeledTree leafFmt nodeFmt t = unlabeledTree t `atop` position (treeLabels leafFmt nodeFmt t)
labeledTree' leafFmt nodeFmt = liftM2 atop (position . treeLabels L leafFmt nodeFmt (0 ^& 0)) unlabeledTree

treeLabels :: _ => Nudge                          -- the correct location of the label relative to the node      
           -> (String -> QDiagram b V2 Double Any)          -- the formatter for the leaf noeds
           -> (Nudge -> String -> QDiagram b V2 Double Any) -- the formatter for the internal nodes
           -> P2 Double                      -- the current location
           -> LabTree                        -- the tree that we are labeling
           -> [(P2 Double, QDiagram b V2 Double Any)]       -- the resulting positioned labels
treeLabels _ leafFmt _ pos (Leaf s) = [(pos, leafFmt s)]
treeLabels n leafFmt nodeFmt pos t@(Node s l r) =
    [(pos, nodeFmt n s)] <>
    treeLabels L leafFmt nodeFmt leftChildPos  l <>
    treeLabels R leafFmt nodeFmt rightChildPos r
  where
    (lOff, rOff) = (subtreeOffsets . subtreeLeafCounts) t
    leftChildPos  = pos .+^ lOff
    rightChildPos = pos .+^ rOff

-- a simple type to remember whether to move the label to the left or to the right
data Nudge = L | R deriving (Eq, Show)

-- *** NODE DATA FORMATTERS ***
-- mostly we would want just simple text functions
-- however, it's also possible to add any sort of customization here:
-- padding, spacing, circles to denote nodes, some sort of extra lines, etc.
empty :: _ => Nudge -> String -> QDiagram b V2 Double Any
empty _ _ = strokeLine emptyLine

nodeLabel :: _ => Nudge -> String -> QDiagram b V2 Double Any
nodeLabel L = nudgeNodeLabel L . setLabelSize . alignedText 1.0 0.0
nodeLabel R = nudgeNodeLabel R . setLabelSize . alignedText 0.0 0.0

leafLabel :: _ => String -> QDiagram b V2 Double Any
leafLabel = nudgeLeafLabel . setLabelSize .baselineText

pt :: _ => QDiagram b V2 Double Any
pt = circle 0.1 # fc red # lw 1

--- *** HELPERS ***

nudgeNodeLabel :: Nudge -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
nudgeNodeLabel L = translate ((-0.2) ^& 0.3)
nudgeNodeLabel R = translate (0.2 ^& 0.3)

nudgeLeafLabel :: QDiagram b V2 Double Any -> QDiagram b V2 Double Any
nudgeLeafLabel = translate ((-0.3) ^& (-1.3))

setLabelSize :: QDiagram b V2 Double Any -> QDiagram b V2 Double Any
setLabelSize = fontSizeL 0.9

frameTree :: QDiagram b V2 Double Any -> QDiagram b V2 Double Any
frameTree = frame 2 . centerXY