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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

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

import "base" Data.Semigroup
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
        makeSerifFont
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

    section # notRefinement ".unnumbered" ? do

        h1 <? hangingHeader 0 1
        h2 <? hangingHeader 1 1.7
        h3 <? hangingHeader 2 2.8

    nav # "#TOC" ? do
        ul ? do
            counterReset "toc-item 0"
            pageBreakBefore "auto"
            pageBreakInside "auto"

            li <? do
                counterIncrement "toc-item"

                marker & do
                    "content" -: "counters(toc-item, \".\", decimal)"

    star ? do
        selection & do
            backgroundColor $ rgba 255 255 0 0.3
            color "#000"

    p ? do
        sym2 margin (em 1) nil

        ".author" & do
            makeFontSize 1.2
            textAlign center

    img ? do
        maxWidth . pct $ 100
        sym borderRadius . em $ 0.2

    h1 <> h2 <> h3 ? do
        color black
        fontWeight normal

    h4 <> h5 <> h6 ? do
        color black
        fontWeight bold

    h1 ? do
        makeFontSize 1.8
        "string-set" -: "chaptitle content()"

    h2 ? do
        makeFontSize 1.4

    h3 ? do
        makeFontSize 1.2

    h4 ? do
        makeFontSize 1

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

    code ? do
        ".amino-acid" & do
            overflowWrap breakWord

    pre <> code <> kbd <> samp ? do
        color "#000"
        makeMonospace
        makeFontSize 0.98

    pre ? do
        whiteSpace T.pre
        whiteSpace preWrap
        wordWrap breakWord
        position relative

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
        pageBreakBefore avoid
        pageBreakInside avoid

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
        "-ms-interpolation-mode" -: "bicubic"
        verticalAlign middle

    figure ? do
        display flex
        textAlign center
        sym2 margin (em 1) nil
        flexFlow F.column F.nowrap
        alignItems center

        img ? do
            borderWidth nil
            sym2 margin nil auto

        figcaption ? do
            floatCaption

    div # ".listing" ? do
        p <? do
            floatCaption

    table ? do
        caption ? do
            floatCaption

    subFigures Nothing

    span ? do
        ".display-locus" & do
            "@data-locus" & do
                "@data-region" & after & do
                    "content" -: "\"(locus \" attr(data-locus) \", between \" attr(data-region) \" on the chromosome)\""
                    "font-style" -: "initial"
                after & do
                    "content" -: "\"(locus \" attr(data-locus) \")\""
                    "font-style" -: "initial"

        ".abbr" & do
            ".acf" & after & do
                "content" -: "\" (\" attr(data-expanded) \")\""

            ".Acf" & after & do
                "content" -: "\" (\" attr(data-expanded) \")\""

            ".acfp" & after & do
                "content" -: "\" (\" attr(data-expanded) \")\""

            ".Acfp" & after & do
                "content" -: "\" (\" attr(data-expanded) \")\""

        ".subfigref" & do
            fontVariant smallCaps

        ".philo" & do
            fontStyle italic

        ".gene" & do
            fontStyle italic

        ".plasmid" & do
            makeMonospace
            makeFontSize 0.8

        ".strain" & do
            whiteSpace nowrap

        ".material" & after & do
            "content" -: "\" (\" attr(data-supplier) \")\""

        ".equipment" & after & do
            "content" -: "\" (\" attr(data-supplier) \")\""

        ".consumable" & after & do
            "content" -: "\" (\" attr(data-supplier) \")\""

        ".researcher" & after & do
            "content" -: "\" (\" attr(data-institution) \", \" attr(data-country) \")\""

        ".todo" & do
            backgroundColor aquamarine
            border solid (px 1) aquamarine

            before & do
                "content" -: "attr(data-todo)"
                position relative
                display inlineBlock
                float floatRight
                backgroundColor aquamarine
                border dashed (px 1) black

            ".experiment" & do
                backgroundColor lightpink
                before & do
                    backgroundColor lightpink

        ".comment" & do
            backgroundColor lavender
            border dashed (px 1) lavender

            before & do
                "content" -: "attr(data-comment)"
                position relative
                display inlineBlock
                float floatRight
                backgroundColor lavender
                border dashed (px 1) black
```

References

```haskell
    div # ".references" |> div ? do
        position relative
        "text-indent" -: "1em hanging"
```

```haskell
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
        color black
        "filter" -: "none !important"
        "-ms-filter" -: "none !important"

        ".ir" & (a # after) ? do
            content normal

    body ? do
        makeFontSize 1
        maxWidth . pct $ 100
        width . mm . pageWidth $ pg

        header # firstChild <? do
            page "title"

        counterReset "chapternum 0"

    nav # "#TOC" ? do
        a # href ? do
            textDecoration none
            color black
            after & do
                -- "content" -: "leader(\" ·    \") target-counter(attr(href), page)"
                "content" -: "\"    ·    \" target-counter(attr(href), page)"

    a ? do
        color slategrey
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

    pre # ".fasta" ? do
        "float" -: "top unless-fit"
        border solid nil "#999"

        code ? do
            border solid (px 1) "#999"
            display block
            "width" -: "fit-content"
            paddingRight . em $ 4
            paddingLeft . em $ 4

    pre <> blockquote ? do
        border solid (px 1) "#999"
        paddingRight . em $ 1
        pageBreakInside avoid

    blockquote ? do
        p # lastOfType # contains "―" <? do
            textAlign . alignSide $ sideRight

    tr <> img <> table <> figure ? do
        pageBreakInside avoid

    img ? do
        "prince-image-resolution" -: "150dpi"

        ".prince" & do
            "prince-image-resolution" -: "300dpi"


    _page ? do
        "size" -: (T.unwords [paperName, "portrait"])

        princeTop ? do
            makeFontSize 0.8
            "content" -: "string(doctitle)"

        star # _left ? do
            margin (mm 15) (mm 20) (mm 15) (mm 10)

            princeBottomLeft ? do
                "content" -: "counter(page)"

        "body" # _left ? do
            princeTop ? do
                content normal

            princeTopRight ? do
                makeFontSize 0.8
                "content" -: "string(doctitle)"

            princeTopLeft ? do
                makeFontSize 0.8
                "content" -: "counter(chapternum) \" · \" string(chaptitle)"

        star # _right ? do
            margin (mm 15) (mm 10) (mm 15) (mm 20)

            princeBottomRight ? do
                "content" -: "counter(page)"

        "body" # _right ? do
            princeTop ? do
                content normal

            princeTopLeft ? do
                makeFontSize 0.8
                "content" -: "string(doctitle)"

            princeTopRight ? do
                makeFontSize 0.8
                "content" -: "string(chaptitle) \" · \" counter(chapternum)"

        star # _first ? do
            princeTop ? do
                content normal

        "landscape" ? do
            princeRotateBody "landscape"
            princeShrinkToFit "auto"

    h1 # ".title" ? do
        "string-set" -: "doctitle content()"

    section ? do

        ".level1" & do
            page "body"
            princePageGroup "start"
            pageBreakBefore "always"

            h1 # firstChild <? do
                makeFontSize 2.5
                position relative
                display block
                textAlign . alignSide $ sideRight

                before & do
                    "content" -: "\"Chapter \" counter(chapternum)"
                    display block
                    position relative
                    textAlign . alignSide $ sideRight
                    left nil
                    marginTop . em $ 6

        ".level2" & do
            pageBreakBefore "always"

    h2 <> h3 ? do
        orphans 3
        widows 3

    h1 <> h2 <> h3 <> h4 <> h5 ? do
        pageBreakAfter avoid

    span ? do
        ".todo" & do
            backgroundColor none
            ".experiment" & do
                backgroundColor lightpink
                before & do
                    backgroundColor lightpink

            before & do
                backgroundColor none

        ".comment" & do
            backgroundColor none
            before & do
                backgroundColor none

    subFigures $ Just pg

    div ? do
        ("id" ^= "tbl:") & do
            position relative

        ".landscape" & do
            page "landscape"

    table ? do

        caption # before <? do
            content normal

hrefReset = after & content normal

hangingHeader level offset = do
    position relative
    res

    before & do
        incr
        "font-style" -: "initial"
        position absolute
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

subFigures :: Maybe PageMM -> Css
subFigures mpg = do
    figure <> (div # ".subfigures")? do
        figcaption ? do
            before & do
                content normal

    sconcat [ figure
            , table
            , div # ".subfigures"
            ] ? do
        "float" -: "top unless-fit"

    div # ".subfigures" ? do
        display flex
        flexFlow F.column F.nowrap

        div # ".subfigrow" <? do
            display flex
            flexFlow F.row F.nowrap
            alignItems center
            "justify-content" -: "space-evenly"
            pure () `maybe` (maxWidth . mm . pageWidth) $ mpg

            forceWidth `mapM_` ([2..10] :: [Int])

        p <? do
            floatCaption
            display block

        figure ? do
            sym margin . em $ 0.1
            width . pct $ 100
            position relative

        table ? do
            maxWidth . pct $ 100
            borderStyle none

            tr # nthChild "odd" ? do
                backgroundColor transparent

        img ? do
            position relative
            zIndex 0

            ".triptych" & do
                "prince-image-resolution" -: "370dpi"

        (img # ".black") |+ figcaption ? do
            color black

        (img # ".grey") |+ figcaption ? do
            backgroundColor black
            opacity 0.5

        figcaption ? do
            position absolute
            color white
            left nil
            top nil
            sym2 padding nil (px 4)
            sym margin nil
            fontWeight bold
            zIndex 5
            fontVariant smallCaps
            sym borderRadius . em $ 0.2

  where
    forceWidth n = "data-n" @= (T.pack . show $ n) & do
        figure <? do
            pure () `maybe` (maxWidth . mm . (/ fromIntegral n) . pageWidth) $ mpg
        img ? do
            pure () `maybe` (width . mm . (/ fromIntegral n) . pageWidth) $ mpg

floatCaption :: Css
floatCaption = do
    makeFontSize 0.8
    fontStyle italic
    sym3 margin nil nil (em 0.8)

```
