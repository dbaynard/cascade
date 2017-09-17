---
title:  Git commit identifier insertion  
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

import           Cascade.Git
import           "base" GHC.Generics
import           "optparse-generic" Options.Generic

data Options w = Options (w ::: FilePath <?> "Output file") (w ::: Text <?> "Git commit identifier")
  -- { outputFile :: w ::: FilePath <?> "Output file" }
  deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options outputFile commit <- unwrapRecord "Output css containing git description"
  gitCommit outputFile commit

```
