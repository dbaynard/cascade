---
title:  Letter CSS style  
author: Davia Baynard  
date:   08 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE PackageImports #-}

module Cascade.Letter (
    module Cascade.Letter
)   where

import "clay" Clay
import qualified "clay" Clay.Flexbox as F
import qualified "clay" Clay.Font as F

import "base" Data.Semigroup
import "text" Data.Text (Text)
import qualified "text" Data.Text.Lazy as TL (Text)
import qualified "text" Data.Text.Lazy.Encoding as TL

import "streaming" Streaming (runResourceT)
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q
```

To generate css, load this module in ghci using

    λ> :m *EmmaMCR.Html.Css

and then use

    λ> renderCss <filename> <clay-css-procedure>

e.g.

    λ> renderCss "/home/<user>/Downloads/mcr.css" mcr

```haskell
renderCss :: FilePath -> Css -> IO ()
renderCss file = runResourceT . Q.writeFile file . Q.fromLazy . TL.encodeUtf8 . render
```

```haskell
letter :: Css
letter = do
        pure ()


```
