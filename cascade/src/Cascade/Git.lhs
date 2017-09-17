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
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Cascade.Git (
    gitCommit
)   where

import "clay" Clay hiding (all, base)

import "text" Data.Text (Text)
import qualified "text" Data.Text.Lazy.Encoding as TL

import "streaming" Streaming (runResourceT)
import qualified "streaming-bytestring" Data.ByteString.Streaming as Q

import Cascade.Print.Page
import Cascade.Print.Prince

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
```

```haskell
gitCommit :: FilePath -> Text -> IO ()
gitCommit file = runResourceT . Q.writeFile file . Q.fromLazy . TL.encodeUtf8 . render . commit
```

```haskell
commit :: Text -> Css
commit i = do
    _page ? do
        princeBottom ? do
            "content" -: i
```
