module Main (main) where

import Diagrams.Backend.SVG.CmdLine

import CombTrees
import Parse

main :: IO ()
main = mainWith . labeledTree . parseString $ "(((((((a,b),c),(d,e)),(f,g)),h),i),k)"
