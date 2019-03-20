---
title:  Cascade  
author: David Baynard  
date:   07 Mar 2019  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
  
...

```haskell
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main
  ( main
  ) where

import           Cascade.Pandoc
import           "base" GHC.Generics
import           "optparse-generic" Options.Generic

data Options w = Options (w ::: FilePath <?> "Output file")
  -- { outputFile :: w ::: FilePath <?> "Output file" }
  deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options outputFile <- unwrapRecord "Output css for pandoc"
  renderCss outputFile pandoc

```
