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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Cascade.Letter (
    module Cascade.Letter
)   where

import "clay" Clay hiding (all, base)
import qualified "clay" Clay as C
import qualified "clay" Clay.Flexbox as F
import qualified "clay" Clay.Font as F
import qualified "clay" Clay.Media as M

import "base" Data.Monoid
import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import qualified "text" Data.Text.Lazy as TL (Text)
import qualified "text" Data.Text.Lazy.Encoding as TL

import "streaming" Streaming (runResourceT)
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q

import Clay.Missing
import Cascade.Base
import Cascade.Fonts
import Cascade.Print.Page
import Cascade.Print.Prince
import Cascade.Rhythm

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

To generate css, load this module in ghci and then use

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
        base a4paper
        letterPrint a4paper


letterPrint :: PageMM -> Css
letterPrint pg@PageSettings{..} = query M.print [] $ do

    _page ? do

        _first & do
            princeTop ? do
                content normal

        princeTop ? do
            -- TODO counter
            "content" -: "string(date)"
            makeFontSize 0.8

        princeBottom ? do
            -- TODO counter
            "content" -: "counter(page) \"/\" counter(pages)"
            makeFontSize 0.8

    header ? do

        section # ".address" ? do

            ".return" & do

                textAlign . alignSide $ sideRight

        p ? do

            sym margin . ex $ 0.3

            ".date" & do

                textAlign . alignSide $ sideRight
                "string-set" -: "date content()" -- TODO

            ".subject" & do

                textAlign . alignSide $ sideCenter
                fontWeight bold

    li # ".cc" ? do

        listStyleType none

    ul # ".cc" ? do

        before & do

            content . stringContent $ "CC:"
            display block
            float floatLeft
            position relative
            left . em $ (-2)

    section # ".signature" ? do

        pageBreakBefore avoid
        pageBreakInside avoid
        breakInside avoid
        widows 2
        orphans 2

        p # ".author" ? do

            paddingTop . ex $ 10

    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        makeFontSize 1

    h1 ? do
        textAlign . alignSide $ sideCenter

    figure ? do
        img <? do
            maxWidth $ pct 80
```
