---
title:  Prince combinators  
author: David Baynard  
date:   08 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Cascade.Print.Prince (
    module Cascade.Print.Prince
)   where

import "text" Data.Text (Text)
import "base" Data.String

import "clay" Clay
import "clay" Clay.Stylesheet (key)

_page :: Selector
_page = "@page"

page :: Text -> Css
page = key "page"

_princePdf :: Selector
_princePdf = "@prince-pdf"

_left :: IsString a => a
_left = ":left"

_right :: IsString a => a
_right = ":right"

_first :: IsString a => a
_first = ":first"

_blank :: IsString a => a
_blank = ":blank"

orphans :: Integer -> Css
orphans = key "orphans"

widows :: Integer -> Css
widows = key "widows"
```

``` { .haskell .ignore }
_not :: Refinement -> Refinement
_not x = ":not(" <> x <> ")"
```

```haskell
princePdfPageLayout :: Text -> Css
princePdfPageLayout = key "prince-pdf-page-layout"

princePdfPageLabel :: Text -> Css
princePdfPageLabel = key "prince-pdf-page-label"

princePdfDestination :: Content -> Css
princePdfDestination = key "prince-pdf-destination"

princeTextReplace :: [(Text, Text)] -> Css
princeTextReplace = key "prince-text-replace" 

princePageGroup :: Text -> Css
princePageGroup = key "prince-page-group"

twoColumn :: Text
twoColumn = "twoColumn"

marginInside :: Size a -> Css
marginInside = key "margin-inside"

marginOutside :: Size a -> Css
marginOutside = key "margin-outside"

princeTop :: Selector
princeTop = "@top"

princeBottom :: Selector
princeBottom = "@bottom"

princeLeft :: Selector
princeLeft = "@left"

princeRight :: Selector
princeRight = "@right"

princeTopLeft :: Selector
princeTopLeft = "@top-left"

princeTopRight :: Selector
princeTopRight = "@top-right"

princeBottomLeft :: Selector
princeBottomLeft = "@bottom-left"

princeBottomRight :: Selector
princeBottomRight = "@bottom-right"

princeLeftTop :: Selector
princeLeftTop = "@left-top"

princeLeftBottom :: Selector
princeLeftBottom = "@left-bottom"

princeRightTop :: Selector
princeRightTop = "@right-top"

princeRightBottom :: Selector
princeRightBottom = "@right-bottom"

princeTopLeftCorner :: Selector
princeTopLeftCorner = "@top-left-corner"

princeTopRightCorner :: Selector
princeTopRightCorner = "@top-right-corner"

princeBottomLeftCorner :: Selector
princeBottomLeftCorner = "@bottom-left-corner"

princeBottomRightCorner :: Selector
princeBottomRightCorner = "@bottom-right-corner"

```
