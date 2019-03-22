---
title:  Git commit description
author: David Baynard  
date:   17 Sep 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Cascade.Git
  ( commit
  ) where

import           "this" Cascade.Print.Prince
import           "this" Cascade.Rhythm
import           "clay" Clay                 hiding (all, base)
import           "text" Data.Text            (Text)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

```haskell
commit :: Text -> Css
commit txt = do
  _page ? do
    princeBottom ? do
      makeFontSize 0.8
      "content" -: "\"" <> txt <> "\""
```
