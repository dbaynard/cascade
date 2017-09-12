---
title:  Pandoc style  
author: David Baynard  
date:   31 Jul 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cascade.Pandoc (
    module Cascade.Pandoc
)   where

import "base" Prelude hiding ((**), rem, span, div)

import "clay" Clay hiding (all, base)
import qualified "clay" Clay as C
import qualified "clay" Clay.Flexbox as F
import qualified "clay" Clay.Font as F
import qualified "clay" Clay.Media as M
import qualified "clay" Clay.Text as T
import qualified "clay" Clay.Pseudo as P

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

``` {.haskell .ignore}
renderCss :: FilePath -> Css -> IO ()
renderCss file = runResourceT . Q.writeFile file . Q.fromLazy . TL.encodeUtf8 . render
```

```haskell
pandoc :: Css
pandoc = do
        base a4paper
        pandocBase
        pandocScreen
        pandocPrint a4paper

pandocBase :: Css
pandocBase = do
```

```haskell
    html ? do
        fontSize . pct $ 100
        overflowY scroll
        textSizeAdjust . pct $ 100

    body ? do
        color "#444"
        -- fontFamily "'Cardo', Georgia, Palatino, 'Palatino Linotype', Times, 'Times New Roman', serif"
        fontFamily ["Linux Libertine"] [serif]
        fontSize . px $ 12
        lineHeight . unitless $ 1.7
        sym padding . em $ 1
        sym margin auto
        maxWidth . em $ 42
        backgroundColor "#fefefe"

    a ? do
        color "#0645ad"
        textDecoration none

        visited & do
              color "#0b0080"

        hover & do
              color "#06e"

        active & do
              color "#faa700"

        focus & do
            --outline dotted thin inherit
            "outline" -: "dotted thin"


        selection & do
            backgroundColor $ rgba 255 255 0 0.3
            color "#0645ad"

    star ? do
        selection & do
            backgroundColor $ rgba 255 255 0 0.3
            color "#000"

    p ? do
        sym2 margin (em 1) nil

        ":contains(\"⁂\")" & do
            textAlign center

    img ? do
        maxWidth . pct $ 100

    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        color "#111"
        lineHeight . pct $ 125
        marginTop . em $ 2
        fontWeight normal

    h4 <> h5 <> h6 ? do
        fontWeight bold

    h1 ? do
        fontSize . em $ 2.5

    h2 ? do
        fontSize . em $ 2

    h3 ? do
        fontSize . em $ 1.5

    h4 ? do
        fontSize . em $ 1.2

    h5 ? do
        fontSize . em $ 1

    h6 ? do
        fontSize . em $ 0.9

    blockquote ? do
        color "#666666"
        sym margin nil
        paddingLeft . em $ 3
        borderLeft solid (em 0.5) "#EEE"

    hr ? do
        display block
        height . px $ 2
        borderWidth nil
        borderTop solid (px 1) "#aaa"
        borderBottom solid (px 1) "#eee"
        sym2 margin (em 1) nil
        sym padding nil

    pre <> code <> kbd <> samp ? do
        color "#000"
        fontFamily [] [monospace]
        -- fontFamily "'courier new', monospace"
        fontSize . em $ 0.98

    pre ? do
        whiteSpace T.pre
        whiteSpace preWrap
        wordWrap breakWord

    b <> strong ? do
        fontWeight bold

    dfn ? do
        fontStyle italic

    ins ? do
        backgroundColor "#ff9"
        color "#000"
        textDecoration none

    mark ? do
        backgroundColor "#ff0"
        color "#000"
        fontStyle italic
        fontWeight bold

    sub <> sup ? do
        fontSize . pct $ 75
        lineHeight nil
        position relative
        verticalAlign vAlignBaseline

    sup ? do
        top . em $ (-0.5)

    sub ? do
        bottom . em $ (-0.25)

    ul <> ol ? do
        sym2 margin (em 1) 0;
        padding nil nil nil (em 2)

    li ** p # lastChild ? do
        marginBottom nil

    (ul ** ul) <> (ol ** ol) ? do
        sym2 margin (em 0.3) nil

    dl ? do
        marginBottom . em $ 1

    dt ? do
        fontWeight bold
        marginBottom . em $ 0.8

    dd ? do
        margin nil nil (em 0.8) (em 2)

        lastChild & do
            marginBottom nil

    img ? do
        borderWidth nil
        "-ms-interpolation-mode" -: "bicubic"
        verticalAlign middle

    figure ? do
        display block
        textAlign center
        sym2 margin (em 1) nil

        img ? do
            borderWidth nil
            sym2 margin nil auto

    figcaption ? do
        fontSize . em $ 0.8
        fontStyle italic
        sym3 margin nil nil (em 0.8)

    table ? do
        marginBottom . em $ 2
        borderBottom solid (px 1) "#ddd"
        borderRight solid (px 1) "#ddd"
        borderSpacing nil
        borderCollapse collapse

        th ? do
            sym2 padding (em 0.2) (em 1)
            backgroundColor "#eee"
            borderTop solid (px 1) "#ddd"
            borderLeft solid (px 1) "#ddd"

        td ? do
            sym2 padding (em 0.2) (em 1)
            borderTop solid (px 1) "#ddd"
            borderLeft solid (px 1) "#ddd"
            verticalAlign vAlignTop

    span ? do
        ".philo" & do
            fontStyle italic

        ".gene" & do
            fontStyle italic

        ".todo" & do
            backgroundColor aquamarine

            before & do
                "content" -: "attr(data-todo)"
                position relative
                display inlineBlock
                float floatRight
                backgroundColor aquamarine
                border solid (px 1) black

        ".comment" & do
            backgroundColor lavender

            before & do
                "content" -: "attr(data-comment)"
                position relative
                display inlineBlock
                float floatRight
                backgroundColor lavender
                border dashed (px 1) black

pandocScreen :: Css
pandocScreen = do
    query M.screen [M.minWidth $ px 480] $ do
        fontSize . px $ 14

    query M.screen [M.minWidth $ px 768] $ do
        fontSize . px $ 16

pandocPrint :: PageMM -> Css
pandocPrint pg@PageSettings{..} = query M.print [] $ do

    star ? do
        "background" -: "transparent !important"
        "color" -: "black !important"
        "filter" -: "none !important"
        "-ms-filter" -: "none !important"

        ".ir" & (a # after) ? do
            content normal

    body ? do
        fontSize . pt $ basePointSize
        maxWidth . pct $ 100
        width . mm . pageWidth $ pg
        counterReset "chapternum"

        header # firstChild <? do
            page "title"

    a ? do
        visited & do
            textDecoration underline

        href & after & do
            "content" -: "\" (\" attr(href) \")\";"

        ("href" ^= "javascript") & after & do
            content normal

        ("href" ^= "#") & after & do
            content normal

    hr ? do
        height . px $ 1
        borderWidth nil
        borderBottom solid (px 1) black

    abbr # title # after ? do
        "content" -: "\" (\" attr(title) \")\";"

    pre <> blockquote ? do
        border solid (px 1) "#999"
        paddingRight . em $ 1
        pageBreakInside avoid

    tr <> img <> table <> figure ? do
        pageBreakInside avoid

    img ? do
        "max-width" -: "100% !important"
        "prince-image-resolution" -: "150dpi"

    _page ? do
        "size" -: (T.unwords [paperName, "portrait"])

        princeTop ? do
            "content" -: "string(doctitle)"

        star # _left ? do
            margin (mm 15) (mm 20) (mm 15) (mm 10)

            princeBottomLeft ? do
                "content" -: "counter(page)"

        star # _right ? do
            margin (mm 15) (mm 10) (mm 15) (mm 20)

            princeBottomRight ? do
                "content" -: "counter(page)"

        star # _first ? do
            princeTop ? do
                content normal

    h1 # ".title" ? do
        "string-set" -: "doctitle content()"

    section ? do

        ".level1" & do
            page "body"
            princePageGroup "start"

        h1 # before ? do
            counterIncrement "chapternum"
            "content" -: "counter(chapternum) \". \""
            "font-style" -: "initial"

    p <> h2 <> h3 ? do
        orphans 3
        widows 3

    h1 <> h2 <> h3 <> h4 <> h5 ? do
        pageBreakAfter avoid

    span ? do
        ".todo" & do
            backgroundColor none
            border solid (px 1) black

            before & do
                backgroundColor none

        ".comment" & do
            backgroundColor none
            border dashed (px 1) black

            before & do
                backgroundColor none

    div # ".subfigures" ? do
        width . pct $ 100
        maxWidth . mm . pageWidth $ pg
```
