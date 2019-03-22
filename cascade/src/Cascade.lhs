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
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Cascade
  ( renderCss
  , Cmd(..)
  , type (>=>)
  , runCmd
  , outFile
  ) where

import           "this" Cascade.Draft
import           "this" Cascade.Git
import           "this" Cascade.Github
import           "this" Cascade.Letter
import           "this" Cascade.Pandoc
import           "clay" Clay
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q
import           "text" Data.Text                                 (Text)
import qualified "text" Data.Text.Lazy.Encoding                   as TL
import           "base" GHC.Generics                              (Generic)
import           "base" GHC.TypeLits                              (Symbol)
import           "streaming-with" Streaming.With                  (writeBinaryFile)

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
type family (>=>) (cmd :: Symbol) w

data Cmd w
  = Pandoc ("outfile" >=> w)
  | Draft ("commit css" >=> w) ("outfile" >=> w)
  | GitInfo ("commit identifier" >=> w) ("outfile" >=> w)
  | Github ("outfile" >=> w)
  | Letter ("outfile" >=> w)
  deriving (Generic)

runCmd
  :: "commit css" >=> w ~ Text
  => "commit identifier" >=> w ~ Text
  => Cmd w -> Css
runCmd (Pandoc _)    = pandoc
runCmd (Draft f _)   = draft f
runCmd (GitInfo t _) = commit t
runCmd (Github _)    = github
runCmd (Letter _)    = letter

outFile :: Cmd w -> "outfile" >=> w
outFile (Pandoc f)    = f
outFile (Draft _ f)   = f
outFile (GitInfo _ f) = f
outFile (Github f)    = f
outFile (Letter f)    = f
```
