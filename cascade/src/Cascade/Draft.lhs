---
title:  Draft css
author: David Baynard  
date:   17 Sep 2017  
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

module Cascade.Draft
  ( renderDraft
  , draft
  ) where

import           "this" Cascade.Fonts
import           "this" Cascade.Rhythm
import           "clay" Clay                                      hiding (all, base)
import qualified "clay" Clay.Media as M
import           "this" Clay.Missing
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q
import           "base" Data.Semigroup
import           "text" Data.Text                                 (Text)
import qualified "text" Data.Text.Lazy.Encoding                   as TL
import           "base" Prelude                                   hiding (div, span)
import           "streaming-with" Streaming.With                  (writeBinaryFile)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

```haskell
renderDraft :: FilePath -> Text -> IO ()
renderDraft file = writeBinaryFile file . Q.fromLazy . TL.encodeUtf8 . render . draft
```

```haskell
draft :: Text -> Css
draft i_ = do
  -- marks
  commit i_
  citeproc
  crossref
  draftGen
  draftPrint

```

``` { .haskell .ignore }
marks :: Css
marks = do
  _page ? do
    "marks" -: "crop cross"
```

```haskell
commit :: Text -> Css
commit i_ = do
  importUrl i_

draftGen :: Css
draftGen = do
  span ? do
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

draftPrint :: Css
draftPrint = query M.print [] $ do
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

citeproc :: Css
citeproc = do
  span ? do
    ".citation" & do
      position relative

      after & do
        citekey "data-cites"

  div # ".references" |> div ? do
    after & do
      bibkey "id"

crossref :: Css
crossref = do
  a ? do
    "href" ^= "#fig:" & do
      after & do
        citekey "href"

    "href" ^= "#tbl:" & do
      after & do
        citekey "href"

  sconcat [ figure |> img
      , div # ".subfigures"
      , div # ("id" ^= "tbl:")
      ] ? do
    after & do
      bibkey "id"

bibkey :: Text -> Css
bibkey key = do
  displaykey key
  position absolute
  right . em $ (-3)
  top . em $ (-1.5)
  pageBreakAfter "avoid"

citekey :: Text -> Css
citekey key = do
  displaykey key
  position relative
  float floatRight
  right . em $ (-3)

displaykey :: Text -> Css
displaykey key = do
  "content" -: ("attr(" <> key <> ")")
  makeMonospace
  makeFontSize $ 0.8
  color grey
  border solid (px 1) grey
  paddingTop nil
  marginBottom nil
```
