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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Cascade.Letter
  ( module Cascade.Letter
  ) where

import           "this" Cascade.Base
import           "this" Cascade.Print.Page
import           "this" Cascade.Print.Prince
import           "this" Cascade.Rhythm
import           "clay" Clay                     hiding (all, base)
import qualified "clay" Clay.Flexbox             as F
import qualified "clay" Clay.Media               as M
import           "this" Clay.Missing

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

To generate css, load this module in ghci and then use

    λ> renderCss <filename> <clay-css-procedure>

e.g.

    λ> renderCss "/home/<user>/Downloads/mcr.css" mcr

``` {.haskell .ignore}
renderCss :: FilePath -> Css -> IO ()
renderCss file = writeBinaryFile file . Q.fromLazy . TL.encodeUtf8 . render
```

```haskell
letter :: Css
letter = do
        base a4paper
        letterPrint a4paper
        invoicePrint
```

# Letter

```haskell
letterPrint :: PageMM -> Css
letterPrint PageSettings{..} = query M.print [] $ do

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

# Invoices

```haskell
invoicePrint :: Css
invoicePrint = query M.print [] $ do
    section # ".payment" ? do
        position relative
        display flex
        flexFlow F.row F.wrap
        alignItems center
        "justify-content" -: "space-evenly"

        p ? do
            position relative
            F.flex 1 0 auto
            textAlign . alignSide $ sideCenter

            ".total" & do
                fontWeight bold

            before & do
                paddingRight $ em 0.5

                ".total" & do
                    "content" -: "\"Total:\""

                ".account" & do
                    "content" -: "\"Account number:\""

                ".sortcode" & do
                    "content" -: "\"Sort code:\""
```

