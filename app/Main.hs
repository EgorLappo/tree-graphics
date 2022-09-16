module Main (main) where

import Options.Applicative

import Control.Lens

import qualified Diagrams.Backend.SVG as SVGB
import qualified Diagrams.Backend.PGF as PGFB

import CombTrees ( unlabeledTree, labeledTree, leafLabeledTree )
import Parse ( parseString, latexize )

data Backend = SVG | PGF deriving (Eq, Show, Read)

data Label = Unlabeled | LeafLabeled | Labeled deriving (Eq, Show)

data AppOptions = AppOptions
    {  _backend :: String
    ,  _label   :: String
    ,  _width   :: Int
    ,  _input   :: String
    ,  _output  :: String }  deriving (Show)
makeLenses ''AppOptions

main :: IO ()
main = do
    options <- execParser cli
    print options



cliArgs :: Parser AppOptions
cliArgs = AppOptions
    <$> strOption (  long "backend"
                    <> short 'b'
                    <> metavar "BACKEND"
                    <> value "SVG"
                    <> help "Choice of rendering backend, with SVG and PGF supported" )
    <*> strOption (  long "label"
                    <> short 'l'
                    <> metavar "LABELS"
                    <> value "unlabeled"
                    <> help "Labeling of the tree, with unlabeled, leaf-labeled, and fully labeled trees supported. The argument should be either of 'labeled', 'leaflabeled', or 'unlabeled', with short options 'l', 'll', and 'ul' also available." )
    <*> option auto (  long "width"
                    <> short 'w'
                    <> metavar "WIDTH"
                    <> value 400
                    <> help "Diagram width" )
    <*> strOption   (  long "input"
                    <> short 'i'
                    <> metavar "INPUTFILE"
                    <> help "Input file, containing a single tree on each line" )
    <*> strOption   (  long "output"
                    <> short 'o'
                    <> metavar "OUTPUTFILES"
                    <> help "Format for output files, written as 'tree%.svg' where percent sign will be replaced by the number of the tree in the input file" )

cli :: ParserInfo AppOptions
cli = info (cliArgs <**> helper)
           (  fullDesc
           <> progDesc "Draw trees from a file containing Newick strings"
           <> header "treegraphics - a script to draw trees for use in mathematical phylogenetics papers" )
