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
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import           Cascade
import           "base" GHC.Generics
import           "optparse-generic" Options.Generic

data Options w = Options
  (w ::: Style <?> "Variant to output (pandoc, draft)")
  (w ::: FilePath <?> "Output file")
  deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

instance ParseField Style
instance ParseFields Style
instance ParseRecord Style

main :: IO ()
main = do
  Options style outputFile <- unwrapRecord "Output css"
  renderCss outputFile (runStyle style)

```
