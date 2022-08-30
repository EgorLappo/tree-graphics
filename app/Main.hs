module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import CombTrees
import Types
import Parse

main :: IO ()
main = mainWith (unlabeledTree (parseString "((((a,b),c),(d,e)),(f,g))") :: Diagram B)
