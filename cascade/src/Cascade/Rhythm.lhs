---
title:  Rhythm  
author: David Baynard  
date:   09 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...


```haskell
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Cascade.Rhythm (
    module Cascade.Rhythm
)   where

import "base" Data.Maybe

import Clay

data RhythmMethod = PositionRelative
                  | SingleDirectionMargin
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | cap height as proportion of font size
capHeight :: Double
capHeight = 0.69

-- | leading as proportion of base font size
leading :: Double
leading = 1.45

-- | base font size in em
baseFontSize :: Double
baseFontSize = 1

baselineHeight :: Double
baselineHeight = leading * baseFontSize

-- | See https://gist.github.com/razwan/10662500 
makeHeight :: Maybe Double -- | Scale. Defaults to 1
           -> RhythmMethod
           -> Double -- | font size in em
           -> Css
makeHeight (fromMaybe 2 -> gScale) rm fSize = do
        fontSize $ em fSize
        lineHeight $ em lHeight
        adjust rm
    where
        shift = fSize * (leading - capHeight) / 2
        rhythm = baselineHeight / gScale
        nlines = (fromIntegral . ceiling $ fSize) * gScale
        lHeight = rhythm * nlines
        lead n | n < 1.5 = baselineHeight
               | otherwise = rhythm
        adjust PositionRelative = do
            position relative
            top $ em shift
        adjust SingleDirectionMargin = do
            paddingTop $ em shift
            marginTop nil
            marginBottom . em $ lead fSize - shift
            paddingBottom nil

makeFontSize :: Double -> Css
makeFontSize = makeHeight Nothing SingleDirectionMargin
```
