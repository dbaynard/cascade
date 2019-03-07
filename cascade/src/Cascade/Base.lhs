---
title:  Base css  
author: David Baynard  
date:   09 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Cascade.Base
  ( module Cascade.Base
  ) where

import           "this" Cascade.Fonts
import           "this" Cascade.Print.Page
import           "this" Cascade.Print.Prince
import           "this" Cascade.Rhythm
import           "clay" Clay                 hiding (all, base)
import qualified "clay" Clay                 as C
import qualified "clay" Clay.Media           as M
import           "this" Clay.Missing
import           "base" Data.Monoid
import qualified "text" Data.Text            as T
import           "base" Prelude              hiding (div)

base :: PageMM -> Css
base pg@PageSettings{..} = query M.print [] $ do
    fonts
    defFont

    html ? do
        fontSize . pt $ basePointSize

    body ? do
        "font-variant" -: "prince-opentype(\"kern\", \"liga\")" -- TODO Prince
        backgroundColor transparent
        makeDefaultFont
        makeFontSize 1
        counterReset "table"
        counterReset "figure"

    p ? do
        widows 3
        orphans 3

    section ? do
        position relative
        princePdfDestination . attrContent $ "id"
        overflowWrap breakWord
        p <> ul <? do
            --maxWidth $ mm 120
            maxWidth . mm . oneColumnWidth $ pg
        ".footnotes" & do
            columnSpan "all"

    img ? do
        maxWidth $ pct 100
        maxHeight . mm $ pageHeight pg
        borderWidth nil
        "-ms-interpolation-mode" -: "bicubic" -- TODO check out
        verticalAlign middle
        ".rotate" & do
            transform . rotate $ deg (-90)
            -- TODO
            -- transformOrigin (location sideBottom) (location sideLeft)
            "transform-origin" -: "bottom left"

    figure <> (div # ".subfigures")? do
        display block
        textAlign center
        sym2 margin (em 1) nil
        counterIncrement "figure"
        position relative

        img <? do
            borderStyle none
            sym2 margin nil auto
            position relative

        figcaption ? do
            makeFontSize 0.8
            fontStyle italic
            sym3 margin nil nil (em 0.8)

            before & do
                -- TODO
                "content" -: "\"Figure \" counter(figure) \":\""
                paddingRight . em $ 0.5

    _page ? do
        "size" -: T.unwords [paperName, "portrait"]
        marginTop . mm $ pageTopSize
        marginOutside . mm $ pageBottomSize
        marginBottom . mm $ pageBottomSize
        marginInside . mm $ pageBottomSize

        princePdfPageLabel "counter(page, lower-alpha)"

    tr <> img <> table <> figure ? do
        pageBreakInside avoid

    table ? do
        margin nil auto (em 2) auto
        borderTop solid (px 2) lightgrey
        borderBottom solid (px 2) lightgrey
        borderSpacing nil
        borderCollapse collapse
        columnSpan C.all
        counterIncrement "table"

        th ? do
            sym2 padding (em 0.2) (em 1)
            fontWeight inherit
            textAlign inherit

        td ? do
            sym2 padding (em 0.2) (em 1)
            verticalAlign vAlignTop

        thead ? do
            textAlign . alignSide $ sideLeft
            fontWeight normal
            makeSmallCaps
            borderBottom solid (px 1) lightgrey

        tbody ? do
            borderStyle none
            tr # nthChild "odd" ? do
                backgroundColor whitesmoke

        caption <? do

            before & do
                -- TODO
                "content" -: "\"Table \" counter(table) \":\""
                paddingRight . em $ 0.5

    blockquote ? do
        sym margin nil
        paddingLeft $ em 3

        notRefinement ".quote" & do
            color . parse $ "#666666"
            borderLeft solid (px 10) (parse "#EEEEEE")

        ".quote" & do
            emojiIconBefore "\xf10d"
            emojiIconAfter "\xf10e"

            before & do
                position relative
                textAlign . alignSide $ sideRight
                float floatLeft
                marginLeft $ em (-1)
                onlyFontAwesome

            after & do
                position relative
                textAlign . alignSide $ sideLeft
                float floatRight
                marginRight $ em (-1)
                bottom $ em 2
                onlyFontAwesome
```

Handle asterisms properly.

```haskell
    p # contains "⁂" ?
        textAlign center

```
