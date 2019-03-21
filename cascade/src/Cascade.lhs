---
title:  Cascade  
author: David Baynard  
date:   20 Mar 2019  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
  
...

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Cascade
  ( renderCss
  , runStyle
  , Style(..)
  ) where

import           "this" Cascade.Draft
import           "this" Cascade.Pandoc
import           "clay" Clay
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q
import qualified "text" Data.Text.Lazy.Encoding                   as TL
import           "base" GHC.Generics                              (Generic)
import           "streaming-with" Streaming.With                  (writeBinaryFile)
import qualified "base" Text.ParserCombinators.ReadP              as R hiding (optional)
import qualified "base" Text.Read                                 as R (lift, readPrec)

```

To generate css, load this module in ghci and then use

    λ> renderCss <filename> <clay-css-procedure>

e.g.

    λ> renderCss "/home/<user>/Downloads/mcr.css" mcr

```haskell
renderCss :: FilePath -> Css -> IO ()
renderCss file = writeBinaryFile file . Q.fromLazy . TL.encodeUtf8 . render
```

```haskell
data Style
  = Pandoc
  | Draft
  deriving (Show, Eq, Generic)

instance Read Style where
  readPrec = R.lift $ R.choice
    [ R.string "pandoc" *> pure Pandoc
    , R.string "draft" *> pure Draft
    ]

runStyle :: Style -> Css
runStyle Pandoc = pandoc
runStyle Draft  = draft "commit.css"
```
