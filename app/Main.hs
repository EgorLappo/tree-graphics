module Main (main) where

import Data.Char (toLower)
import Control.Monad ( zipWithM_ )
import Data.List (intercalate)
import Data.List.Split ( splitOn )

import Options.Applicative

import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as SVGB
import qualified Diagrams.Backend.PGF as PGFB

import CombTrees ( unlabeledTree, labeledTree, leafLabeledTree )
import Parse ( parseString, latexize )
import Text.PrettyPrint.ANSI.Leijen (text)


data Backend = SVG | PGF deriving (Eq, Show, Read)

data Label = Unlabeled | LeafLabeled | Labeled deriving (Eq, Show)

data AppOptions = AppOptions
    {  backend :: String
    ,  label   :: String
    ,  width   :: Int
    ,  input   :: String
    ,  output  :: String }  deriving (Show)

main :: IO ()
main =
    do opts <- execParser cli
       eitherSplit (readBackend $ backend opts) $ \curBackend ->
        do eitherSplit (readLabel $ label opts) $ \curLabel ->
            do eitherSplit (readOutputFormat $ output opts) $ \oFmt ->
                do file <- readFile $ input opts
                   zipWithM_ (draw curBackend curLabel (width opts) . oFmt) [1..] (lines file)


draw :: Backend -> Label -> Int -> String -> String -> IO ()
draw SVG = drawSVG
draw PGF = drawPGF

drawSVG :: Label -> Int -> String -> String -> IO ()
drawSVG lab w out tree = case lab of
    Unlabeled -> (render . unlabeledTree . parseString) tree
    Labeled -> (render . labeledTree . parseString) tree
    LeafLabeled -> (render . leafLabeledTree . parseString) tree
  where render = SVGB.renderPretty out (D.mkWidth $ fromIntegral w)

drawPGF :: Label -> Int -> String -> String -> IO ()
drawPGF lab w out tree = case lab of
    Unlabeled -> (render . unlabeledTree . latexize . parseString) tree
    Labeled -> (render . labeledTree . latexize . parseString) tree
    LeafLabeled -> (render . leafLabeledTree . latexize . parseString) tree
  where render = PGFB.renderPGF out (D.mkWidth $ fromIntegral w)

cliArgs :: Parser AppOptions
cliArgs = AppOptions
    <$> strOption (  long "backend"
                    <> short 'b'
                    <> metavar "BACKEND"
                    <> value "SVG"
                    <> help "Choice of rendering backend, with SVG and PGF supported. The argument should be either 'svg' or 'pgf', case-insensitive. Default value is 'SVG'." )
    <*> strOption (  long "label"
                    <> short 'l'
                    <> metavar "LABELS"
                    <> value "unlabeled"
                    <> help "Labeling of the tree, with unlabeled, leaf-labeled, and fully labeled trees supported. The argument should be either of 'labeled', 'leaflabeled', or 'unlabeled', case-insensitive. Short options 'l', 'll', and 'ul' also available. Default value is 'unlabeled'." )
    <*> option auto (  long "width"
                    <> short 'w'
                    <> metavar "WIDTH"
                    <> value 400
                    <> help "Diagram width. Default value is 400." )
    <*> strOption   (  long "input"
                    <> short 'i'
                    <> metavar "INPUTFILE"
                    <> help "Input file, containing a single tree on each line." )
    <*> strOption   (  long "output"
                    <> short 'o'
                    <> metavar "OUTPUTFILES"
                    <> help "Format for output files, written as 'tree%.svg' where percent sign will be replaced by the number of the tree in the input file." )

cli :: ParserInfo AppOptions
cli = info (cliArgs <**> helper)
           (  fullDesc
           <> progDesc "Draw trees from a file containing Newick strings"
           <> headerDoc (Just $ text "treegraphics 0.1.0\nEgor Lappo, egor@ccrma.stanford.edu\nA script to draw trees for use in mathematical phylogenetics papers.") )

readBackend :: String -> Either String Backend
readBackend s = case map toLower s of
    "svg" -> Right SVG
    "pgf" -> Right PGF
    _     -> Left $ "The backend you requested is not available: " ++ s

readLabel :: String -> Either String Label
readLabel s = case map toLower s of
    "unlabeled"   -> Right Unlabeled
    "ul"          -> Right Unlabeled
    "labeled"     -> Right Labeled
    "l"           -> Right Labeled
    "leaflabeled" -> Right LeafLabeled
    "ll"          -> Right LeafLabeled
    _             -> Left $ "The labeling you requested is not available: " ++ s

readOutputFormat :: String -> Either String (Int -> String)
readOutputFormat s
    | '%' `notElem` s                 = Left $ "Output format does not contain the `%` character: " ++ s
    | length (filter (=='%') s) > 1 = Left $ "More than one `%` character in the output format: " ++ s
    | otherwise = Right $ \i -> intercalate (show i) $ splitOn "%" s


-- I think anything like ExceptT is too much for me, so here is a simple helper function
-- if we have Left, just print the left side,
-- if we have Right, run the action given by the second argument
eitherSplit :: Either String b -> (b -> IO ()) -> IO ()
eitherSplit (Left a) _ = putStrLn a
eitherSplit (Right b) f = f b