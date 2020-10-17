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
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE ViewPatterns      #-}

module Clay.Missing
  ( module Clay.Missing
  ) where

import           "clay" Clay
import           "clay" Clay.Selector   (Predicate (..), Refinement (..), selectorFromText)
import           "clay" Clay.Stylesheet (key)
import           "text" Data.Text       (Text)
import qualified "text" Data.Text       as T

tt :: Selector
tt = selectorFromText "tt"

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
breakBefore :: Text -> Css
breakBefore = key "break-before"

-- should take avoid
breakAfter :: Text -> Css
breakAfter = key "break-after"

-- should take avoid
columnBreakBefore :: Text -> Css
columnBreakBefore = key "column-break-before"

-- should take avoid
columnBreakAfter :: Text -> Css
columnBreakAfter = key "column-break-after"

transformOrigin :: Location -> Location -> Css
transformOrigin a_ b_ = key "transform-origin" (a_ ! b_)

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
    refined = pred_ `foldMap` ns
    pred_ (Id text) = "#" <> text
    pred_ (Class text) = "." <> text
    pred_ (Attr text) = ":" <> text
    pred_ (AttrVal attr_ val) = attr_ <> "@=" <> val
    pred_ (AttrBegins attr_ val) = attr_ <> "^=" <> val
    pred_ (AttrEnds attr_ val) = attr_ <> "$=" <> val
    pred_ (AttrContains attr_ val) = attr_ <> "*=" <> val
    pred_ (AttrSpace attr_ val) = attr_ <> "~=" <> val
    pred_ (AttrHyph attr_ val) = attr_ <> "|=" <> val
    pred_ (Pseudo text) = ":" <> text
    pred_ (PseudoFunc text texts) = ":" <> text <> T.unwords texts
    pred_ (PseudoElem text) = "::" <> text

marker :: Refinement
marker = pseudo ":marker"
```
