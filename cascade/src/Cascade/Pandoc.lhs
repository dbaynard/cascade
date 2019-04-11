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
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Cascade.Pandoc
  ( module Cascade.Pandoc
  ) where

import           Cascade.Base
import           Cascade.Fonts
import           Cascade.Print.Page
import           Cascade.Print.Prince
import           Cascade.Rhythm
import           "clay" Clay            hiding (all, base)
import qualified "clay" Clay.Elements   as E (em)
import qualified "clay" Clay.Flexbox    as F
import qualified "clay" Clay.Media      as M
import           Clay.Missing
import qualified "clay" Clay.Text       as T
import           "errors" Control.Error
import           "base" Data.Semigroup
import           "text" Data.Text       (Text)
import qualified "text" Data.Text       as T
import           "base" Prelude         hiding (div, rem, span, (**))

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
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

    ".level1" & hangingHeader h1 0 1
    ".level2" & hangingHeader h2 1 1.7
    ".level3" & hangingHeader h3 2 2.8

  sconcat
    [ nav # "#TOC"
    , div # ".list-of-figures"
    , div # ".list-of-tables"
    ] ? do
      pageBreakBefore "always"

      ul ? do
        counterReset "toc-item 0"
        pageBreakBefore "auto"
        pageBreakInside "auto"

        li <? do
          counterIncrement "toc-item"

  nav # "#TOC" ? do

    stringSet "marker-prefix" "\"\""

    li # marker ? do
        "content" -: "string(marker-prefix) counters(toc-item, \".\", decimal)"

    ul # notRefinement "first-child" <? do
      stringSet "marker-prefix" "\"A\""

  sconcat
    [ div # ".list-of-figures"
    , div # ".list-of-tables"
    ] ? do
      li # marker ? do
        "content" -: "none"
      li ? a <? do
        position relative

        before & do
          "content" -: "attr(data-float-no) \".\""
          position absolute
          textAlign $ alignSide sideRight
          textIndent . indent $ em (-4)
          left $ em 3

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

    ".abbr" & after & do
      ".acf" & do
        "content" -: "\" (\" attr(data-expanded) \")\""

      ".Acf" & do
        "content" -: "\" (\" attr(data-expanded) \")\""
        textTransform capitalize

      ".acfp" & do
        "content" -: "\" (\" attr(data-expanded) \"s)\""

      ".Acfp" & do
        "content" -: "\" (\" attr(data-expanded) \"s)\""
        textTransform capitalize

      "@data-longplural" & do
        ".acfp" & do
          "content" -: "\" (\" attr(data-longplural) \")\""

        ".Acfp" & do
          "content" -: "\" (\" attr(data-longplural) \")\""

      ".bare" & do
        ".acf" & do
          "content" -: "\", \" attr(data-expanded)"

        ".Acf" & do
          "content" -: "\", \" attr(data-expanded)"

        ".acfp" & do
          "content" -: "\", \" attr(data-expanded) \"s\""

        ".Acfp" & do
          "content" -: "\", \" attr(data-expanded) \"s\""

        "@data-longplural" & do
          ".acfp" & do
            "content" -: "\", \" attr(data-longplural)"

          ".Acfp" & do
            "content" -: "\", \" attr(data-longplural)"

    emphasized italic

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

    ".subfigref" & do
      fontVariant normal

  div ? do

    ".todo" & do
      display none

    ".comment" & do
      display none
```

References

```haskell
  div # ".references" ? do
    div <? do
      position relative
      "text-indent" -: "1em hanging"

    a # href ? hrefReset
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

    counterReset "chapternum 0"

    header # firstChild <? do
      page "title"
      display flex
      flexFlow F.row F.wrap
      "justify-content" -: "space-evenly"
      alignItems baseline

      div ? do
        "#crests" & do
          display flex
          flexFlow F.row F.nowrap
          alignItems center
          "justify-content" -: "space-around"
          F.flex 1 0 auto

          img <? do
            height (mm 30)

      p ? do

        ".author" & do
          F.flex 1 1 (pct 100)

        ".date" & do
          F.flex 1 0 auto

        ".college" & do
          F.flex 1 0 auto
          textAlign . alignSide $ sideRight

        "#declaration" & do
          clear both
          F.flex 1 0 (pct 100)

      img # "#cover" ? do
        width (pct 100)
        "prince-image-resolution" -: "300dpi"

  sconcat
    [ nav # "#TOC"
    , div # ".list-of-figures"
    , div # ".list-of-tables"
    ] ? do
      ul ? li <? a # href <? do
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
      makeFontSize 0.8
      makeMonospace

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
    border solid nil "#999"
    paddingRight . em $ 1
    pageBreakInside avoid

  blockquote ? do
    p # lastOfType # contains "―" <? do
      textAlign . alignSide $ sideRight

  tr <> img <> table <> figure <> div # ".listing" ? do
    pageBreakInside avoid

  img ? do
    "prince-image-resolution" -: "150dpi"

    ".prince" & do
      "prince-image-resolution" -: "300dpi"

    ".triptych" & do
      "prince-image-resolution" -: "200dpi"

      ".paired" & do
        "prince-image-resolution" -: "370dpi"

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
        "content" -: "string(chapter-label) \" · \" string(chaptitle)"

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
        "content" -: "string(chaptitle) \" · \" string(chapter-label)"

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
      stringSet "chapter-label" "counter(chapternum)"

      "#sec:appendix" & do
        page "appendix"

      "@data-label" & do
        stringSet "chapter-label" "attr(data-label)"

      h1 # firstChild <? do
        makeFontSize 2.5
        position relative
        display block
        textAlign . alignSide $ sideRight

        before & do
          "content" -: "\"Chapter \" string(chapter-label)"
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

  subFigures $ Just pg

  div ? do
    ("id" ^= "tbl:") & do
      position relative

    ".landscape" & do
      page "landscape"

  table ? do
    princeBookmarkLevel 5

    caption # before <? do
      content normal

hrefReset :: Css
hrefReset = after & content normal

hangingHeader :: Selector -> Int -> Double -> Css
hangingHeader h level offset = do
  res
  incr

  h <? do
    position relative

    before & do
      "font-style" -: "initial"
      position absolute
      textAlign . alignSide $ sideRight
      left $ em (-0.5)
      textIndent . indent . em $ 0 - offset
  where
    res = pure () `fromMaybe` do
      sec_ <- secs `atZ` (level + 1)
      pure $ counterReset . T.unwords $ [sec_, "0"]

    incr = pure () `fromMaybe` do
      sec_ <- secs `atZ` level
      pure $ do
        counterIncrement sec_
        h <? before & do
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

levelcounter :: Text -> Text
levelcounter "chapternum" = "string(chapter-label)"
levelcounter sec_         = mconcat ["counter(", sec_, ")"]

subFigures :: Maybe PageMM -> Css
subFigures mpg = do
  figure <> (div # ".subfigures")? do
    figcaption ? do
      before & do
        content normal

  sconcat
    [ section # ".level1"
    , section # ".level2"
    , section # ".level3"
    , section # ".level4"
    , section # ".level5"
    , section # ".level6"
    ] ? do
      figure <? do
        princeBookmarkLevel 5

  sconcat
    [ figure
    , div # ".subfigures"
    , table
    , div # ".listing"
    ] ? do
      "float" -: "top unless-fit"

      ".full-page" & do
        "float" -: "top"

      ".bump" & do
        "float" -: "top"

  div # ".subfigures" ? do
    display flex
    flexFlow F.column F.nowrap
    princeBookmarkLevel 5
    princeBookmarkLabel "attr(data-label)"

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
      fontVariant normal
      sym borderRadius . em $ 0.2

  where
  forceWidth :: Int -> Css
  forceWidth n = "data-n" @= (T.pack . show $ n) & do
    figure <? do
      pure () `maybe` (maxWidth . mm . (/ fromIntegral n) . pageWidth) $ mpg
    img ? do
      pure () `maybe` (width . mm . (/ fromIntegral n) . pageWidth) $ mpg

floatCaption :: Css
floatCaption = do
  makeFontSize 0.8
  fontStyle italic
  emphasized normal
  sym3 margin nil nil (em 0.8)
  textAlign . alignSide $ sideLeft

emphasized :: FontStyle -> Css
emphasized styl = do
  ".philo" & do
    fontStyle styl

  ".gene" & do
    fontStyle styl

  ".re-enzyme" & do
    fontStyle styl

  E.em ? do
    fontStyle styl
```
