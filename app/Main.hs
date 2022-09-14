module Main (main) where

import Diagrams.Backend.SVG.CmdLine

import CombTrees
import Types
import Parse

main :: IO ()
main = mainWith . labeledTree . parseString $ "(((((((a,b),c),(d,e)),(f,g)),e),f),g)"
