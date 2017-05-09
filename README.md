---
title:  Print css files  
author: David Baynard  
date:   09 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

Make Css files using Haskell’s `clay` library.

# Install

    > stack install markdown-unlit cascade

# Run

To produce css for a letter, run

    > cascade _filename_

but note that due to a [shortcoming](https://github.com/sebastiaanvisser/clay/issues/137) of clay it is necessary to fix some issues with the output file.

There’s quite clearly no point to a program that produces the same, faulty file, in any location.
The idea is to have a series of selectable output files, in a customisable manner.
One thing at a time, though.
