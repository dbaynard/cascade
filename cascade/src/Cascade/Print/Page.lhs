---
title:  Page settings for css  
author: David Baynard  
date:   08 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Cascade.Print.Page (
    module Cascade.Print.Page
)   where

import "base" Control.Arrow
import "text" Data.Text (Text)

import "clay" Clay
import qualified "clay" Clay.Text as C
import qualified "clay" Clay.Flexbox as F
import qualified "clay" Clay.Media as M

data PageSettings a = PageSettings {
              paperName :: Text
            , basePointSize :: a
            , paperHeight :: a
            , paperWidth :: a
            , frontPageTopSize :: a
            , frontPageRightSize :: a
            , frontPageBottomSize :: a
            , frontPageLeftSize :: a
            , pageTopSize :: a
            , pageOutSize :: a
            , pageBottomSize :: a
            , pageInSize :: a
            } deriving (Eq, Ord, Show, Read)

pageHeight, pageWidth :: Num a => PageSettings a -> a
pageHeight PageSettings{..} = paperHeight - pageTopSize - pageBottomSize
pageWidth PageSettings{..} = paperWidth - pageOutSize - pageInSize

twoColumnWidth, oneColumnWidth, columnGapWidth :: Floating a => PageSettings a -> a
twoColumnWidth = (0.25 *) . paperWidth
oneColumnWidth = (0.8 *) . paperWidth
columnGapWidth = (0.08 *) . paperWidth

type PageMM = PageSettings Double

a4paper :: PageMM
a4paper = PageSettings{..}
    where
        paperName = "A4"
        basePointSize = 10
        paperWidth = 210
        paperHeight = 295
        frontPageTopSize = 30
        frontPageRightSize = 30
        frontPageBottomSize = 20
        frontPageLeftSize = 50
        pageTopSize = 20
        pageOutSize = 15
        pageBottomSize = 20
        pageInSize = 30

a5paper :: PageMM
a5paper = PageSettings{..}
    where
        paperName = "A5"
        basePointSize = 8
        paperWidth = 148
        paperHeight = 210
        frontPageTopSize = 20
        frontPageRightSize = 20
        frontPageBottomSize = 10
        frontPageLeftSize = 40
        pageTopSize = 15
        pageOutSize = 10
        pageBottomSize = 10
        pageInSize = 20

scaleToA4 :: (Double, Double) -> (Double, Double)
scaleToA4 = (/ paperWidth a4paper) *** (/ paperHeight a4paper)
```
