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
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import           Cascade
import           "optparse-generic" Options.Generic

type instance "outfile" >=> w = (w ::: FilePath <?> "Output file")
type instance "commit css" >=> w = (w ::: Text <?> "Name of commit css file")
type instance "commit identifier" >=> w = (w ::: Text <?> "Git commit identifier")

instance ParseRecord (Cmd Wrapped)
deriving instance Show (Cmd Unwrapped)

main :: IO ()
main = do
  cmd <- unwrapRecord "Output css"
  app cmd
```
