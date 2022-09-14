module Main (main) where

import Diagrams.Prelude
import qualified Diagrams.Backend.SVG as SVG
import qualified Diagrams.Backend.PGF as PGF
import qualified Diagrams.Backend.SVG.CmdLine as SVGC
import qualified Diagrams.Backend.PGF.CmdLine as PGFC

import CombTrees
import Parse

main :: IO ()
main = SVGC.mainWith diagram
    where t = parseString $ "(((((((a,b),c),(d,e)),(f,g)),h),i),k)"
          diagram :: Diagram SVG.B 
          diagram = labeledTree t
