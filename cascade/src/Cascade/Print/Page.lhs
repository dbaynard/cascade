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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module Cascade.Print.Page
  ( module Cascade.Print.Page
  ) where

import           "base" Control.Arrow
import           "text" Data.Text     (Text)

data PageSettings a = PageSettings
  { paperName           :: Text
  , sided               :: Sided
  , basePointSize       :: a
  , paperHeight         :: a
  , paperWidth          :: a
  , frontPageTopSize    :: a
  , frontPageRightSize  :: a
  , frontPageBottomSize :: a
  , frontPageLeftSize   :: a
  , pageTopSize         :: a
  , pageOutSize         :: a
  , pageBottomSize      :: a
  , pageInSize          :: a
  , lineSpacing         :: Maybe a -- ^ Override the line spacing
  , chapterStartRecto   :: Bool
  } deriving (Eq, Ord, Show, Read)

data Sided
  = SingleSided
  | DoubleSided
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data PageSide
  = Verso
  | Recto
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

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
    sided = DoubleSided
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
    lineSpacing = Nothing
    chapterStartRecto = True

a5paper :: PageMM
a5paper = PageSettings{..}
  where
    paperName = "A5"
    sided = DoubleSided
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
    lineSpacing = Nothing
    chapterStartRecto = False

thesis :: PageMM
thesis = a4paper
  { basePointSize = 12
  , lineSpacing = Just 1.7
  , chapterStartRecto = True
  }

scaleToA4 :: (Double, Double) -> (Double, Double)
scaleToA4 = (/ paperWidth a4paper) *** (/ paperHeight a4paper)
```
