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
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Cascade.Draft
  ( renderDraft
  , draft
  ) where

import "base" Prelude hiding (span, div)

import "base" Data.Semigroup

import "clay" Clay hiding (all, base)

import "text" Data.Text (Text)
import qualified "text" Data.Text.Lazy.Encoding as TL

import "streaming-with" Streaming.With (writeBinaryFile)
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q

import Clay.Missing
import Cascade.Rhythm
import Cascade.Fonts
import Cascade.Print.Page
import Cascade.Print.Prince

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

```haskell
renderDraft :: FilePath -> Text -> IO ()
renderDraft file = writeBinaryFile file . Q.fromLazy . TL.encodeUtf8 . render . draft
```

```haskell
draft :: Text -> Css
draft i = do
    -- marks
    commit i
    citeproc
    crossref

marks :: Css
marks = do
    _page ? do
        "marks" -: "crop cross"

commit :: Text -> Css
commit i = do
    importUrl i

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
