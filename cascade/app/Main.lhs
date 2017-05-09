
```haskell
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
    main
)   where

import "base" GHC.Generics

import Cascade.Letter

import "optparse-generic" Options.Generic

data Options w = Options (w ::: FilePath <?> "Output file")
    -- { outputFile :: w ::: FilePath <?> "Output file" }
    deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
    Options outputFile <- unwrapRecord "Output css for letter"
    renderCss outputFile letter

```
