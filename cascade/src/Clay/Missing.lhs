---
title:  Missing Clay functions  
author: David Baynard  
date:   09 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}

module Clay.Missing (
    module Clay.Missing
)   where


import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import "base" Data.Semigroup

import "clay" Clay
import "clay" Clay.Selector (Refinement(..), Predicate(..), text)
import "clay" Clay.Stylesheet (key)

tt :: Selector
tt = text "tt"

textSizeAdjust :: Size a -> Css
textSizeAdjust (plain . unValue . value -> sz) = do
    "-webkit-text-size-adjust" -: sz
    "-moz-text-size-adjust" -: sz
    "-ms-text-size-adjust" -: sz
    "text-size-adjust" -: sz

fontVariantLigatures :: Text -> Css
fontVariantLigatures ligs = do
    "-webkit-font-variant-ligatures" -: ligs
    "font-variant-ligatures" -: ligs

-- should allow normal
fontKerning :: Text -> Css
fontKerning kern = do
    "-webkit-font-kerning" -: kern
    "-moz-font-kerning" -: kern
    "-ms-font-kerning" -: kern
    "font-kerning" -: kern

-- should take list of enums
fontFeatureSettings :: [Text] -> Css
fontFeatureSettings (T.intercalate ", " -> enums) = do
    "-webkit-font-feature-settings" -: enums
    "-moz-font-feature-settings" -: enums
    "-ms-font-feature-settings" -: enums
    "font-feature-settings" -: enums

-- should take 1 or 2 parameters
counterReset :: Text -> Css
counterReset =
    key "counter-reset"

counterIncrement :: Text -> Css
counterIncrement =
    key "counter-increment"
```

``` { .haskell .ignore }
-- TODO
counterContent :: Text -> Content
counterContent x = "counter(" <> x <> ")"
```

```haskell
columns :: Integer -> Size a -> Css
columns n w = key "columns" (n ! w)

columnGap :: Size a -> Css
columnGap = key "column-gap"

columnSpan :: Value -> Css
columnSpan = key "column-span"

avoid :: Text
avoid = "avoid"

-- should take avoid
breakInside :: Text -> Css
breakInside = key "break-inside"

-- should take avoid
pageBreakInside :: Text -> Css
pageBreakInside = key "page-break-inside"

-- should take avoid
breakBefore :: Text -> Css
breakBefore = key "break-before"

-- should take avoid
pageBreakBefore :: Text -> Css
pageBreakBefore = key "page-break-before"

-- should take avoid
breakAfter :: Text -> Css
breakAfter = key "break-after"

-- should take avoid
pageBreakAfter :: Text -> Css
pageBreakAfter = key "page-break-after"

-- should take avoid
columnBreakBefore :: Text -> Css
columnBreakBefore = key "column-break-before"

-- should take avoid
columnBreakAfter :: Text -> Css
columnBreakAfter = key "column-break-after"

transformOrigin :: Location -> Location -> Css
transformOrigin a b = key "transform-origin" (a ! b)

-- TODO enable auto
hyphens :: Text -> Css
hyphens h = do
    "-webkit-hyphens" -: h
    "-moz-hyphens" -: h
    "hyphens" -: h

contains :: Text -> Refinement
contains n = func "contains" [quote n]

notRefinement :: Refinement -> Refinement
notRefinement (unFilter -> ns) = func "not" [refined]
    where
        refined = pred `foldMap` ns
        pred (Id text) = "#" <> text
        pred (Class text) = "." <> text
        pred (Attr text) = ":" <> text
        pred (AttrVal attr val) = attr <> "@=" <> val
        pred (AttrBegins attr val) = attr <> "^=" <> val
        pred (AttrEnds attr val) = attr <> "$=" <> val
        pred (AttrContains attr val) = attr <> "*=" <> val
        pred (AttrSpace attr val) = attr <> "~=" <> val
        pred (AttrHyph attr val) = attr <> "|=" <> val
        pred (Pseudo text) = ":" <> text
        pred (PseudoFunc text texts) = ":" <> text <> T.unwords texts
        pred (PseudoElem text) = "::" <> text

marker :: Refinement
marker = pseudo ":marker"
```
