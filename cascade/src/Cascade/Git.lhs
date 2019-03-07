---
title:  Git commit description
author: David Baynard  
date:   17 Sep 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Cascade.Git
  ( gitCommit
  ) where

import           "this" Cascade.Print.Prince
import           "this" Cascade.Rhythm
import           "clay" Clay                                      hiding (all, base)
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q
import           "text" Data.Text                                 (Text)
import qualified "text" Data.Text.Lazy.Encoding                   as TL
import           "streaming-with" Streaming.With                  (writeBinaryFile)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

```haskell
gitCommit :: FilePath -> Text -> IO ()
gitCommit file = writeBinaryFile file . Q.fromLazy . TL.encodeUtf8 . render . commit
```

```haskell
commit :: Text -> Css
commit txt = do
  _page ? do
    princeBottom ? do
      makeFontSize 0.8
      "content" -: "\"" <> txt <> "\""
```
