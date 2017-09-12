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
{-# LANGUAGE ApplicativeDo #-}

module Cascade.Pandoc (
    module Cascade.Pandoc
)   where

import "base" Prelude hiding ((**), rem, span, div)

import "errors" Control.Error

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
        overflowY scroll
        textSizeAdjust . pct $ 100

    body ? do
        color "#444"
        -- fontFamily "'Cardo', Georgia, Palatino, 'Palatino Linotype', Times, 'Times New Roman', serif"
        fontFamily ["Linux Libertine"] [serif]
        makeFontSize 1
        -- lineHeight . unitless $ 1.7
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

        ".author" & do
            makeFontSize 1.2
            textAlign center

    p ? do
        sym2 margin (em 1) nil

        ":contains(\"⁂\")" & do
            textAlign center

    img ? do
        maxWidth . pct $ 100

    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        color "#111"
        marginTop . em $ 2
        fontWeight normal

    h4 <> h5 <> h6 ? do
        fontWeight bold

    h1 ? do
        makeFontSize 2.5

    h2 ? do
        makeFontSize 2

    h3 ? do
        makeFontSize 1.5

    h4 ? do
        makeFontSize 1.2

    h5 ? do
        makeFontSize 1

    h6 ? do
        makeFontSize 0.9

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
        makeMonospace
        makeFontSize 0.98

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
        makeFontSize 0.75
        -- lineHeight nil
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
        makeFontSize 0.8
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
    query M.screen [] $ do
        table ? do
            marginBottom . em $ 2
            borderBottom solid (px 1) "#ddd"
            borderRight solid (px 1) "#ddd"
            borderSpacing nil
            borderCollapse collapse

            th <> td ? do
                sym2 padding (em 0.2) (em 1)
                borderTop solid (px 1) "#ddd"
                borderLeft solid (px 1) "#ddd"

            th ? do
                backgroundColor "#eee"

            td ? do
                verticalAlign vAlignTop

    query M.screen [M.minWidth $ px 480] $ do
        makeFontSize 1.4

    query M.screen [M.minWidth $ px 768] $ do
        makeFontSize 1.6

pandocPrint :: PageMM -> Css
pandocPrint pg@PageSettings{..} = query M.print [] $ do

    star ? do
        backgroundColor transparent
        color black
        "filter" -: "none !important"
        "-ms-filter" -: "none !important"

        ".ir" & (a # after) ? do
            content normal

    body ? do
        makeFontSize 1
        maxWidth . pct $ 100
        width . mm . pageWidth $ pg
        counterReset "chapternum"

        header # firstChild <? do
            page "title"

    nav # "#TOC" ? do
        ul <? do
            counterReset "toc-chapter 0"

            li <? do
                counterIncrement "toc-chapter"

                marker & do
                    "content" -: levelcounter "toc-chapter"

        ul |> li |> ul <? do
            counterReset "toc-section 0"

            li <? do
                counterIncrement "toc-section"

                marker & do
                    "content" -: levelcounters "\".\"" ["toc-chapter", "toc-section"]

        ul |> li |> ul |> li |> ul <? do
            counterReset "toc-subsection 0"

            li <? do
                counterIncrement "toc-subsubsection"

                marker & do
                    "content" -: levelcounters "\".\"" ["toc-chapter", "toc-section", "toc-subsection"]

        a # href ? do
            textDecoration none
            after & do
                "content" -: "leader(\" ·    \") target-counter(attr(href), page)"

    a ? do
        visited & do
            textDecoration underline

        href & after & do
            "content" -: "\" (\" attr(href) \")\";"

        "href" ^= "javascript" & hrefReset
        "href" ^= "#" & hrefReset
        "href" *= "doi.org" & do
            hrefReset
            makeMonospace

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
            makeFontSize 0.8
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
            pageBreakBefore "always"

        h1 <? hangingHeader 0 1
        h2 <? hangingHeader 1 1.4 
        h3 <? hangingHeader 2 2.1

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

    table ? do
        margin nil auto (em 2) auto
        borderTop solid (px 2) "#d3d3d3"
        borderBottom solid (px 2) "#d3d3d3"
        borderSpacing nil
        borderCollapse collapse
        columnSpan allValue
        counterIncrement "table"
        -- page wide

        th ? do
            padding (em 0.2) (em 1) (em 0.2) (em 1)
            fontWeight inherit
            textAlign inherit

        td ? do
            padding (em 0.2) (em 1) (em 0.2) (em 1);
            verticalAlign vAlignTop

        thead ? do
            textAlign . alignSide $ sideLeft
            fontWeight normal
            makeSmallCaps
            borderBottom solid (px 1) "#d3d3d3"

        tbody ? do
            borderStyle none

            tr # nthChild "odd" ? do
                backgroundColor "#f5f5f5"

        caption # before <? do
            -- "content" -: "\"Table \" counter(table) \":\""
            paddingRight . em $ 0.5

hrefReset = after & content normal

hangingHeader level offset = do
    position relative
    res

    before & do
        incr
        "font-style" -: "initial"
        position relative
        float floatLeft
        width nil
        textAlign . alignSide $ sideRight
        left . em $ 0 - offset
  where
    res = pure () `fromMaybe` do
        sec <- secs `atZ` (level + 1)
        pure $ counterReset . T.unwords $ [sec, "0"]
    incr = pure () `fromMaybe` do
        sec <- secs `atZ` level
        pure $ do
            counterIncrement sec
            "content" -: (levelcounters "\".\"" . take (level+1) $ secs)
    secs = 
        [ "chapternum"
        , "sectionnum"
        , "subsectionnum"
        , "subsubsectionnum"
        , "subsubsubsectionnum"
        , "paragraphnum"
        , "subparagraphnum"
        ]

levelcounters :: Text -> [Text] -> Text
levelcounters sep = T.intercalate sep . fmap levelcounter

levelcounter sec = mconcat ["counter(", sec, ")"]
```
