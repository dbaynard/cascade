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

import "clay" Clay
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
letter = let PageSettings{..} = a4paper in do
        fonts
        defFont

        html ? do
            fontSize . pt $ basePointSize

        body ? do
            "font-variant" -: "prince-opentype(\"kern\", \"liga\")" -- TODO Prince
            backgroundColor transparent

        section ? do
            princePdfDestination . attrContent $ "id"
            overflowWrap breakWord
            p <> ul <? do
                --maxWidth $ mm 120
                maxWidth . mm . oneColumnWidth $ a4paper
            ".footnotes" & do
                columnSpan "all"

        letterPrint a4paper


letterPrint :: PageMM -> Css
letterPrint pg@PageSettings{..} = query M.print [] $ do

    body ? do
        makeDefaultFont
        makeFontSize 1

    _page ? do
        "size" -: T.unwords [paperName, "portrait"]
        marginTop . mm $ pageTopSize
        marginOutside . mm $ pageBottomSize
        marginBottom . mm $ pageBottomSize
        marginInside . mm $ pageBottomSize

        princePdfPageLabel "counter(page, lower-alpha)"

    header ? do

        section # ".address" ? do

            ".return" & do

                textAlign . alignSide $ sideRight

        p ? do

            sym margin . ex $ 0.3

            ".date" & do

                textAlign . alignSide $ sideRight

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

        p # ".author" ? do

            paddingTop . ex $ 10
```
