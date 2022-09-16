# Drawing trees with `Haskell` and `diagrams`

This project contains some code to simplify the graphics creation process for my papers in phylogenetics and combinatorics. 

**This is a work in progress!**

## Usage

To install the program, make sure you have a Haskell toolchain on your computer, including Stack. Then, in the root directory of the project, run
`stack install --local-bin-path .` to compile an executable and move it to the root of the project.

Here is the help message for the program:

```
treegraphics 0.1.0
Egor Lappo, egor@ccrma.stanford.edu
A script to draw trees for use in mathematical phylogenetics papers.

Usage: treegraphics-exe [-b|--backend BACKEND] [-l|--label LABELS] 
                        [-w|--width WIDTH] (-i|--input INPUTFILE)
                        (-o|--output OUTPUTFILES)
  Draw trees from a file containing Newick strings

Available options:
  -b,--backend BACKEND     Choice of rendering backend, with SVG and PGF
                           supported. The argument should be either 'svg' or
                           'pgf', case-insensitive. Default value is 'SVG'.
  -l,--label LABELS        Labeling of the tree, with unlabeled, leaf-labeled,
                           and fully labeled trees supported. The argument
                           should be either of 'labeled', 'leaflabeled', or
                           'unlabeled', case-insensitive. Short options 'l',
                           'll', and 'ul' also available. Default value is
                           'unlabeled'.
  -w,--width WIDTH         Diagram width. Default value is 400.
  -i,--input INPUTFILE     Input file, containing a single tree on each line.
  -o,--output OUTPUTFILES  Format for output files, written as 'tree%.svg' where
                           percent sign will be replaced by the number of the
                           tree in the input file.
  -h,--help                Show this help text
```

Trees are encoded in a simplified Newick format, like this: `"((a,b),c)"`. Internal node labels are optional, but please provide them if you want to draw fully-labeled trees. Internal nodes are labeled by a string following the closing paren of the node, like this: `"((a,b)d,c)e"`.

The input file should be a text file with a single tree on each line. Each diagram would be rendered into a separate output file. The format for output files is simple: define the desired file path as you would regularly, but add a `'%'` character to mark a place to insert a number. 

## Examples

After installing, try running

`./treegraphics-exe -b PGF -i input.txt -o output/tree%.pgf`

or 

`./treegraphics-exe -b SVG -l ll -i input.txt -o output/tree%.svg`

to generate example trees.
