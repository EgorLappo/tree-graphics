# Drawing trees with `Haskell` and `diagrams`

This project contains some code to simplify the graphics creation process for my papers in phylogenetics and combinatorics. 

This is a work in progress! Usage guide and explanations are coming soon.

## Usage

Here is the help message for the program:

```
treegraphics - a script to draw trees for use in mathematical phylogenetics
papers

Usage: treegraphics-exe [-b|--backend BACKEND] [-l|--label LABELS] 
                        [-w|--width WIDTH] (-i|--input INPUTFILE)
                        (-o|--output OUTPUTFILES)
  Draw trees from a file containing Newick strings

Available options:
  -b,--backend BACKEND     Choice of rendering backend, with SVG and PGF
                           supported
  -l,--label LABELS        Labeling of the tree, with unlabeled, leaf-labeled,
                           and fully labeled trees supported. The argument
                           should be either of 'labeled', 'leaflabeled', or
                           'unlabeled', with short options 'l', 'll', and 'ul'
                           also available.
  -w,--width WIDTH         Diagram width
  -i,--input INPUTFILE     Input file, containing a single tree on each line
  -o,--output OUTPUTFILES  Format for output files, written as 'tree%.svg' where
                           percent sign will be replaced by the number of the
                           tree in the input file
  -h,--help                Show this help text
```

Trees are encoded in a simplified Newick format, like this: "((a,b),)