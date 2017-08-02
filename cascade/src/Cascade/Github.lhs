---
title:  Github pandoc style  
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

module Cascade.Github (
    module Cascade.Github
)   where

import "base" Prelude hiding ((**))

import "clay" Clay hiding (all, base)
import qualified "clay" Clay as C
import qualified "clay" Clay.Flexbox as F
import qualified "clay" Clay.Font as F
import qualified "clay" Clay.Media as M
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
github :: Css
github = do
        base a4paper
        githubCompat
        githubPrint a4paper

githubCompat :: Css
githubCompat = do

    article <> aside <> details <> figcaption
      <> figure <> footer <> header <> hgroup
      <> main_ <> nav <> section <> summary ? do
        display block

    audio <> canvas <> video ? do
        display inlineBlock

    audio # C.not "[controls]" ? do -- TODO
        display none
        height nil

    "[hidden]" <> template ? do
        display none

    -- Remove the gray background color from active links in IE 10.
    a ? do
        backgroundColor transparent

        -- Address `outline` inconsistency between Chrome and other browsers.
        focus & do
            outlineStyle dotted

    -- Improve readability when focused and also mouse hovered in all browsers.
        active & do
            outlineWidth nil

        hover & do
            outlineWidth nil

    -- Address variable `h1` font-size and margin within `section` and `article`
    -- contexts in Firefox 4+, Safari 5, and Chrome.
    h1 ? do
        makeFontSize 2

    -- Address styling not present in IE 8/9, Safari 5, and Chrome.
    abbr # "title" ? do -- TODO
        borderBottom dotted (px 1) inherit

    -- Address style set to `bolder` in Firefox 4+, Safari 5, and Chrome.
    b <> strong ? do
        fontWeight bold

    -- Address styling not present in Safari 5 and Chrome.
    dfn ? do
        fontStyle italic

    -- Address differences between Firefox and other browsers.
    hr ? do
        boxSizing contentBox
        height nil

    -- Address styling not present in IE 8/9.
    mark ? do
        backgroundColor "#ff0"
        color black

    -- Correct font family set oddly in Safari 5 and Chrome.
    code <> kbd <> pre <> samp ? do
        makeMonospace
        makeFontSize 1

    -- Improve readability of pre-formatted text in all browsers.
    pre ? do
        whiteSpace preWrap

    -- Set consistent quote types. -- TODO
    q ? do
        "quotes" -: "\"\\201C\" \"\\201D\" \"\\2018\" \"\\2019\""

    -- Address inconsistent and variable font size in all browsers.
    small ? do
        makeFontSize 0.8

    -- Prevent `sub` and `sup` affecting `line-height` in all browsers.
    sub <> sup ? do
        makeFontSize 0.75 -- TODO
        -- fontSize . pct $ 75
        -- lineHeight nil
        position relative
        verticalAlign vAlignBaseline

    sup ? do
        top . em $ (-0.5)

    sub ? do
        bottom . em $ (-0.25)

    -- 1. Set default font family to sans-serif.
    -- 2. Prevent iOS text size adjust after orientation change, without disabling user zoom.
    html ? do
        makeDefaultFont
        textSizeAdjust . pct $ 100

    -- Remove default margin.
    body ? do
        sym margin nil

    -- Embedded content

    -- Remove border when inside `a` element in IE 8/9.
    img ? do
        borderWidth nil

    -- Correct overflow displayed oddly in IE 9.
    svg # notRefinement P.root ? do
        overflow hidden

    -- Address margin not present in IE 8/9 and Safari 5.
    figure ? do
        sym margin nil

    -- Forms

    -- Define consistent border, margin, and padding.
    fieldset ? do
        border solid (px 1) "#c0c0c0"
        sym2 margin nil (px 2)
        sym3 padding (em 0.35) (em 0.625) (em 0.75)

    legend ? do
        -- 1. Correct `color` not being inherited in IE 8/9.
        borderWidth nil
        -- 2. Remove padding so people aren't caught out if they zero out fieldsets.
        sym padding nil

    button <> input <> select <> textarea ? do
        -- 1. Correct font family not being inherited in all browsers.
        fontFamily ["inherit"] []
        -- 2. Correct font size not being inherited in all browsers.
        fontSize . pct $ 100
        -- 3. Address margins set differently in Firefox 4+, Safari 5, and Chrome.
        sym margin nil

    -- Address Firefox 4+ setting `line-height` on `input` using `!important` in the UA stylesheet.
    button <> input ? do
        lineHeight normal

    -- Address inconsistent `text-transform` inheritance for `button` and `select`.
    -- All other form control elements do not inherit `text-transform` values.
    -- Correct `button` style inheritance in Chrome, Safari 5+, and IE 8+.
    -- Correct `select` style inheritance in Firefox 4+ and Opera.

    button <> select ? do
        textTransform none

    -- 1. Avoid the WebKit bug in Android 4.0.* where (2) destroys native `audio` and `video` controls.
    button
      <> (html ** (input # ("type" @= "button")))
      <> (input # ("type" @= "reset"))
      <> (input # ("type" @= "submit")) ? do
        -- 2. Correct inability to style clickable `input` types in iOS.
        "appearance" -: "button" -- TODO
        "-webkit-appearance" -: "button" -- TODO
        -- 3. Improve usability and consistency of cursor style between image-type `input` and others.
        cursor pointer

    -- Re-set default cursor for disabled elements.
    (button # "disabled") <> (html ** input # "disabled") ? do
        cursor cursorDefault

    (input # ("type" @= "checkbox")) <> (input # ("type" @= "radio")) ? do
        -- 1. Address box sizing set to `content-box` in IE 8/9/10.
        boxSizing borderBox
        -- 2. Remove excess padding in IE 8/9/10.
        sym padding nil

    input # ("type" @= "search") ? do
        -- 1. Address `appearance` set to `searchfield` in Safari 5 and Chrome.
        "appearance" -: "textfield" -- TODO
        "-webkit-appearance" -: "textfield" -- TODO
        boxSizing contentBox
        -- 2. Address `box-sizing` set to `border-box` in Safari 5 and Chrome (include `-moz` to future-proof).
        boxSizing contentBox

    -- Remove inner padding and search cancel button in Safari 5 and Chrome on OS X.
    (input # ("type" @= "search") # "::-webkit-search-cancel-button") -- TODO
      <> (input # ("type" @= "search") # "::-webkit-search-decoration") ? do -- TODO
        "appearance" -: "none" -- TODO
        "-webkit-appearance" -: "none" -- TODO

    -- Remove inner padding and border in Firefox 4+.
    (button # "::-moz-focus-inner") <> (input # "::-moz-focus-inner") ? do -- TODO
        borderWidth nil
        sym padding nil

    textarea ? do
        -- 1. Remove default vertical scrollbar in IE 8/9.
        overflow auto
        -- 2. Improve readability and alignment in all browsers.
        verticalAlign vAlignTop
        width . pct $ 90

    -- Tables
    table ? do
        borderCollapse collapse
        borderSpacing nil

    ".go-top" & do
        position fixed
        bottom . em $ 2
        right . em $ 2
        textDecoration none
        backgroundColor "#E0E0E0"
        fontSize . px $ 12 -- TODO
        sym padding . em $ 1
        display inline

githubPrint :: PageMM -> Css
githubPrint pg@PageSettings{..} = query M.print [] $ do

    body ? do
        backgroundColor white
        makeDefaultFont
        fontWeight lighter
        lineHeight . unitless $ 1.4
        borderWidth nil

    img <> pre <> blockquote <> table <> figure ? do
        pageBreakInside avoid

    pre ? do
        backgroundColor white

        code ? do
            overflow visible

    code ? do
        backgroundColor white
        color "#333"
        sym2 padding nil (em 0.2)
        border solid (px 1) "#dedede"

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

        p ? do

            sym margin . ex $ 0.3

            ".date" & do

                textAlign . alignSide $ sideRight
                "string-set" -: "date content()" -- TODO

    figure ? do
        img <? do
            maxWidth $ pct 80
```
