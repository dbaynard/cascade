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
  ( app
  , renderCss
  , renderFile
  , Cmd(..)
  , type (>=>)
  , runCmd
  ) where

import           "this" Cascade.Draft
import           "this" Cascade.Git
import           "this" Cascade.Github
import           "this" Cascade.Letter
import           "this" Cascade.Pandoc
import           "this" Cascade.Print.Page                        (a4paper, thesis)
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
app ::
  ( "commit css" >=> w ~ Text
  , "commit identifier" >=> w ~ Text
  )
  => Cmd w -> IO ()
app = renderCss . runCmd

renderFile :: FilePath -> Css -> IO ()
renderFile file = writeBinaryFile file . Q.fromLazy . TL.encodeUtf8 . render

renderCss :: Css -> IO ()
renderCss = Q.stdout . Q.fromLazy . TL.encodeUtf8 . render
```

```haskell
type family (>=>) (cmd :: Symbol) w

data Cmd w
  = Pandoc
  | Draft ("commit css" >=> w)
  | GitInfo ("commit identifier" >=> w)
  | Github
  | Letter
  | Thesis
  deriving (Generic)

runCmd ::
  ( "commit css" >=> w ~ Text
  , "commit identifier" >=> w ~ Text
  )
  => Cmd w -> Css
runCmd Pandoc      = pandoc a4paper
runCmd (Draft f)   = draft f
runCmd (GitInfo t) = commit t
runCmd Github      = github
runCmd Letter      = letter
runCmd Thesis      = pandoc thesis
```
