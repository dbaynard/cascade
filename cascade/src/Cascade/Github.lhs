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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Cascade.Github
  ( github
  ) where

import           "this" Cascade.Base
import           "this" Cascade.Fonts
import           "this" Cascade.Print.Page
import           "this" Cascade.Print.Prince
import           "this" Cascade.Rhythm
import           "clay" Clay                                      hiding (all, base)
import qualified "clay" Clay                                      as C
import qualified "clay" Clay.Media                                as M
import           "this" Clay.Missing
import qualified "clay" Clay.Pseudo                               as P
import qualified "clay" Clay.Text                                 as T
import           "base" Data.Monoid
import           "base" Prelude                                   hiding (rem, span, (**))

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

```haskell
github :: Css
github = do
    base a4paper
    githubCompat
    githubBase
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

githubBase :: Css
githubBase = do
```

``` { .haskell .ignore }
  star # C.not "#mkdbuttons" ? do -- TODO
    sym margin nil
    sym padding nil
```

```haskell
  query M.screen [M.maxDeviceWidth . px $ 1090 ] $ do

    html <> body ? do
      sym margin auto
      paddingRight . em $ 1
      paddingLeft . em $ 1
      maxWidth . em $ 44
      color black

    (C.div <> nav) # "#TOC" ? do
      position static

  query M.screen [M.minDeviceWidth . px $ 1090 ] $ do

    html <> body ? do
      sym margin auto
      -- margin auto auto auto (em 21)
      paddingRight . em $ 1
      paddingLeft . em $ 1
      maxWidth . em $ 44
      color black

    (C.div <> nav) # "#TOC" ? do

      display block
      position fixed
      left nil
      top . vh $ 10

    body # ".inverted" ? do

      color "#eee" -- TODO important
      borderColor "#555"
      "boxShadow" -: "none" -- TODO

    star # ".inverted" ? do

      backgroundColor "#0b2531"
      backgroundColor "#252a2a"

      body ? do
        backgroundColor "#252a2a"

      a ? do
        color "#acd1d5"

        selection & do
            backgroundColor $ rgba 255 230 102 0.6

      body <> (hr ** (star # ".inverted") ** p) <>
        td <> li <> h1 <> h2 <> h3 <> h4 <> h5 <> h6 <>
        th <> (star # ".math") <> caption <> dd <> dt <>
        blockquote ? do
        color "#eee" -- TODO important
        borderColor "#555"
        "boxShadow" -: "none" -- TODO

      td <> th ? do
        backgroundColor "#333"

      hr ? do
        borderColor "#777"
        borderWidth . px $ 1 -- TODO important

    star # selection ? do
      backgroundColor $ rgba 157 193 200 0.5

    h1 # selection ? do
      backgroundColor $ rgba 45 156 208 0.3

    h2 # selection ? do
      backgroundColor $ rgba 90 182 224 0.3

    (h3 <> h4 <> h5 <> h6 <> li <> ol) # selection ? do
      backgroundColor $ rgba 133 201 232 0.3

    code # selection ? do
      backgroundColor $ rgba 0 0 0 0.7
      color "#eee"

      span # selection ? do
        backgroundColor $ rgba 0 0 0 0.7 -- TODO !important
        color "#eee" -- TODO !important

    a # selection ? do
      backgroundColor $ rgba 255 230 102 0.2

    (td <> th <> caption) # selection ? do

      backgroundColor $ rgba 180 237 95 0.5

  body ? do
    makeSansFont
    makeFontSize 1
    paddingLeft . px $ 3
    paddingRight . px $ 3
    "-webkit-font-smoothing"  -: "subpixel-antialiased" -- TODO
    backgroundColor white
    sym borderRadius . px $ 3

    (star # firstChild) <? do
      marginTop nil -- TODO !important

    (star # lastChild) <? do
      marginBottom nil -- TODO !important

    query M.screen [] $ do
      "box-shadow" -: "0 0 0 1px #cacaca, 0 0 0 4px #eee"
      -- boxShadows -- TODO
      --     [ (nil, nil, nil, px 1, "#cacaca")
      --     , (nil, nil, nil, px 4, "#eee")
      --     ]

    -- Custom

    makeFontSize 1.2
    paddingLeft . px $ 30
    paddingRight . px $ 30
    sym margin . px $ 15

  p ? do
    sym2 margin (em 1) nil

  a ? do
    color "#4183c4"
    textDecoration none

    firstChild & do
      h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        marginTop nil
        paddingTop nil

  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
    sym3 margin (px 20) nil (px 10)
    paddingRight nil
    paddingLeft nil
    fontWeight bold
    "-webkit-font-smoothing" -: "subpixel-antialiased" -- TODO
    cursor cursorText

  ((h1 <> h2 <> h3 <> h4 <> h5 <> h6) |+ p) <>
    ((ul <> ol) ** li # firstChild) ? do
    marginTop nil

  h1 ? do
    makeFontSize 2.8
    color black

  h2 ? do
    makeFontSize 2.4
    borderBottom solid (px 1) "#ccc"
    color black

  h3 ? do
    makeFontSize 1.8
    color "#333"

  h4 ? do
    makeFontSize 1.6
    color "#333"

  h5 ? do
    makeFontSize 1.4
    color "#333"

  h6 ? do
    makeFontSize 1.4
    color "#777"

  p <> blockquote <> table <> pre ? do
    sym2 margin (px 15) nil

  ul <> ol ? do
    paddingLeft . px $ 30

  ol ** li ** (ul # firstOfType) ? do
    marginTop nil

  hr ? do
    backgroundRepeat repeatX
    backgroundColor transparent
    backgroundImage . url $
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAECA\ 
      \YAAACtBE5DAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPA\ 
      \AAAyJpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj\ 
      \0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG\ 
      \1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIF\ 
      \hNUCBDb3JlIDUuMC1jMDYwIDYxLjEzNDc3NywgMjAxMC8wMi8xMi0xNzozMj\ 
      \owMCAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3Ln\ 
      \czLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcm\ 
      \lwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYm\ 
      \UuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLm\ 
      \NvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS\ 
      \5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG\ 
      \9vbD0iQWRvYmUgUGhvdG9zaG9wIENTNSBNYWNpbnRvc2giIHhtcE1NOkluc3\ 
      \RhbmNlSUQ9InhtcC5paWQ6OENDRjNBN0E2NTZBMTFFMEI3QjRBODM4NzJDMj\ 
      \lGNDgiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6OENDRjNBN0I2NTZBMT\ 
      \FFMEI3QjRBODM4NzJDMjlGNDgiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZW\ 
      \Y6aW5zdGFuY2VJRD0ieG1wLmlpZDo4Q0NGM0E3ODY1NkExMUUwQjdCNEE4Mz\ 
      \g3MkMyOUY0OCIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDo4Q0NGM0E3OT\ 
      \Y1NkExMUUwQjdCNEE4Mzg3MkMyOUY0OCIvPiA8L3JkZjpEZXNjcmlwdGlvbj\ 
      \4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/Pq\ 
      \qezsUAAAAfSURBVHjaYmRABcYwBiM2QSA4y4hNEKYDQxAEAAIMAHNGAzhkPO\ 
      \lYAAAAAElFTkSuQmCC"
    backgroundPosition $ positioned nil nil
    border none nil none
    color "#ccc"
    height . px $ 4
    sym padding nil

  body ? do
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 <? do
      firstChild & do
        marginTop nil
        paddingTop nil

    (h1 # firstChild) |+ h2 ? do
      marginTop nil
      paddingTop nil

  dl ? do
    sym padding nil

    dt ? do
      makeFontSize 0.8
      fontWeight bold
      sym3 margin (px 15) nil (px 5)

      firstChild & do
        sym padding nil

      marginChildren

    dd ? do
      sym3 margin nil nil (px 15)
      sym2 padding nil (px 15)

      marginChildren

  blockquote ? do
    borderLeft solid (px 4) "#DDD"
    sym2 padding nil (px 15)
    color "#777"

    marginChildren

  table ? do
    borderCollapse collapse
    borderSpacing nil
    makeFontSize 1
    "font" -: "inherit" -- TODO

    th ? do
      fontWeight bold
      border solid (px 1) "#ccc"
      sym2 padding (px 6) (px 13)

    td ? do
      border solid (px 1) "#ccc"
      sym2 padding (px 6) (px 13)

    tr ? do
      border solid (px 1) "#ccc"
      backgroundColor white

      nthChild "2n" & do
        backgroundColor "#f8f8f8"

  img ? do
    maxWidth . pct $ 100

  code <> tt ? do
    sym2 margin nil (px 2)
    sym2 padding nil (px 5)
    whiteSpace nowrap
    border solid (px 1) "#eaeaea"
    sym borderRadius . px $ 3
    makeMonospace
    makeFontSize 1
    color "#333"

  pre ? do
    code <? do
      sym margin nil
      sym padding nil
      whiteSpace T.pre

    code <> tt ? do
      backgroundColor transparent
      borderWidth nil

    backgroundColor "#f8f8f8"
    border solid (px 1) "#ccc"
    makeFontSize 0.9
    overflow auto
    sym2 padding (px 6) (px 10)
    sym borderRadius . px $ 3

  (star # ".highlight") ** pre ? do
    pure ()

  (star # ".poetry") ** pre ? do
    makeSerifFont
    fontStyle italic
    makeFontSize 1
    display block
    marginLeft . em $ 1

    code ? do
      wordBreak breakAll
      wordWrap breakWord
      hyphens "auto" -- TODO
      whiteSpace preWrap

  sup <> sub <> (a # ".footnote") ? do
    makeFontSize 0.6
    height nil
    verticalAlign vAlignSuper

  sub ? do
    verticalAlign vAlignSub
    top . px $ (-1)

  C.span # ".comment-start" ? do

    before & do
      "content" -: "\"(\" attr(date) \")\""
      display block
      textAlign . alignSide $ sideRight

    after & do
      "content" -: "\"--\" attr(author)";
      display block
      textAlign . alignSide $ sideRight

    position relative
    float floatRight
    border solid (px 1) black  -- TODO border solid thin black 
    backgroundColor bisque
    sym padding . px $ 6
    userSelect none
    fontSizeCustom medium
    fontWeight normal
    "font-family" -: "initial" -- TODO
    "font-style" -: "initial" -- TODO

  star # ".highlight" ? do

    ".c" & do
      color "#998"
      fontStyle italic

    ".err" & do
      color "#a61717"
      backgroundColor "#e3d2d2"

    ".k" & do
      fontWeight bold

    ".o" & do
      fontWeight bold

    ".cm" & do
      color "#998"
      fontStyle italic

    ".cp" & do
      color "#999"
      fontWeight bold

    ".c1" & do
      color "#998"
      fontStyle italic

    ".cs" & do
      color "#999"
      fontWeight bold;
      fontStyle italic

    ".gd" & do
      color "#000"
      backgroundColor "#fdd"

    ".gd .x" & do
      color "#000"
      backgroundColor "#faa"

    ".ge" & do
      fontStyle italic

    ".gr" & do
      color "#a00"

    ".gh" & do
      color "#999"

    ".gi" & do
      color "#000"
      backgroundColor "#dfd"

    ".gi .x" & do
      color "#000"
      backgroundColor "#afa"

    ".go" & do
      color "#888"

    ".gp" & do
      color "#555"

    ".gs" & do
      fontWeight bold

    ".gu" & do
      color "#800080"
      fontWeight bold

    ".gt" & do
      color "#a00"

    ".kc" & do
      fontWeight bold

    ".kd" & do
      fontWeight bold

    ".kn" & do
      fontWeight bold

    ".kp" & do
      fontWeight bold

    ".kr" & do
      fontWeight bold

    ".kt" & do
      color "#458"
      fontWeight bold

    ".m" & do
      color "#099"

    ".s" & do
      color "#d14"

    ".na" & do
      color "#008080"

    ".nb" & do
      color "#0086b3"

    ".nc" & do
      color "#458"
      fontWeight bold

    ".no" & do
      color "#008080"

    ".ni" & do
      color "#800080"

    ".ne" & do
      color "#900"
      fontWeight bold

    ".nf" & do
      color "#900"
      fontWeight bold

    ".nn" & do
      color "#555"

    ".nt" & do
      color "#000080"

    ".nv" & do
      color "#008080"

    ".ow" & do
      fontWeight bold

    ".w" & do
      color "#bbb"

    ".mf" & do
      color "#099"

    ".mh" & do
      color "#099"

    ".mi" & do
      color "#d14"

    ".sc" & do
      color "#d14"

    ".sd" & do
      color "#d14"

    ".s2" & do
      color "#d14"

    ".se" & do
      color "#d14"

    ".sh" & do
      color "#d14"

    ".si" & do
      color "#d14"

    ".sx" & do
      color "#d14"

    ".sr" & do
      color "#009926"

    ".s1" & do
      color "#d14"

    ".ss" & do
      color "#990073"

    ".bp" & do
      color "#999"

    ".vc" & do
      color "#008080"

    ".vg" & do
      color "#008080"

    ".vi" & do
      color "#008080"

    ".il" & do
      color "#099"

    ".gc" & do
      color "#999"
      backgroundColor "#eaf2f5"

  star # ".type-csharp" ** star # ".highlight" ? do

    ".k" & do
      color "#00F"

    ".kt" & do
      color "#00F"

    ".nf" & do
      color black
      fontWeight normal

    ".nc" & do
      color "#2b91af"

    ".nn" & do
      color black

    ".s" & do
      color "#a31515"

    ".sc" & do
      color "#a31515"

  where
  marginChildren = do
    star # firstChild <? do
      marginTop nil

    star # lastChild <? do
      marginBottom nil


githubPrint :: PageMM -> Css
githubPrint PageSettings{..} = query M.print [] $ do

  body ? do
    backgroundColor white
    makeDefaultFont
    fontWeight lighter
    lineHeight . unitless $ 1.4
    borderWidth nil

  img <> pre <> blockquote <> table <> figure ? do
    breakInside avoid

  pre ? do
    backgroundColor white

    code ? do
      overflow visible
      overflowWrap breakWord
      whiteSpace preWrap

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
