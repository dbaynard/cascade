---
title:  CSS Font handling  
author: David Baynard  
date:   09 May 2017  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Cascade.Fonts
  ( module Cascade.Fonts
  ) where

import           "clay" Clay
import           "this" Clay.Missing
import           "text" Data.Text    (Text)

makeMonospace :: Css
makeMonospace = fontFamily ["Roboto Mono", "Noto Emoji", "Font Awesome"] [monospace]

makeDefaultFont :: Css
makeDefaultFont = makeSerifFont

makeSerifFont :: Css
makeSerifFont = fontFamily ["Cardo", "Noto Emoji", "Font Awesome", "STIX Two Math"] []

makeSansFont :: Css
makeSansFont = fontFamily ["Lato Light", "Roboto Light", "STIX Two Math"] [sansSerif]

onlyFontAwesome :: Css
onlyFontAwesome = fontFamily ["Font Awesome"] []

makeSmallCaps :: Css
makeSmallCaps = do
  fontVariant smallCaps
  letterSpacing $ em 0.06

modf :: Css
modf = do
  makeMonospace
  "font-variant" -: "initial" -- TODO
  fontSize $ pct 80

emojiIcon :: Text -> Css
emojiIcon icon_ = do
  makeDefaultFont
  color black
  contents
    [ stringContent icon_
    ]

emojiIconBefore :: Text -> Css
emojiIconBefore icon_ = before & do
  emojiIcon icon_
  paddingRight $ em 0.3

emojiIconAfter :: Text -> Css
emojiIconAfter icon_ = after & do
  emojiIcon icon_
  paddingLeft $ em 0.3

mono :: Text -> Css
mono patt = contains patt & do
  modf
  after & content normal
  before & content normal

defFont :: Css
defFont = do
    defFace serif "Cardo"
    defFace sansSerif "Lato Light"
    defFace monospace "Roboto Mono Regular"
  where
    defFace gen src_ = fontFace $ do
      fontFamily [] [gen]
      --fontFaceSrc [FontFaceSrcLocal src] -- TODO
      "src" -: "prince-lookup(" <> src_ <> ")"

fonts :: Css
fonts = do
    makeFace "Cardo" bold normal "fonts/Cardo-Bold.ttf" TrueType Nothing
    makeFace "Cardo" normal italic "fonts/Cardo-Italic.ttf" TrueType Nothing
    makeFace "Cardo" normal normal "fonts/Cardo-Regular.ttf" TrueType Nothing
    makeFace "Noto Emoji" normal normal "fonts/NotoEmoji-Regular.ttf" TrueType Nothing
    makeFace "Roboto Mono" (weight 100) italic "fonts/RobotoMono-ThinItalic.ttf" TrueType Nothing
    makeFace "Roboto Mono" (weight 100) normal "fonts/RobotoMono-Thin.ttf" TrueType Nothing
    makeFace "Roboto Mono" (weight 300) italic "fonts/RobotoMono-LightItalic.ttf" TrueType Nothing
    makeFace "Roboto Mono" (weight 300) normal "fonts/RobotoMono-Light.ttf" TrueType Nothing
    makeFace "Roboto Mono" (weight 500) italic "fonts/RobotoMono-MediumItalic.ttf" TrueType Nothing
    makeFace "Roboto Mono" (weight 500) normal "fonts/RobotoMono-Medium.ttf" TrueType Nothing
    makeFace "Roboto Mono" bold italic "fonts/RobotoMono-BoldItalic.ttf" TrueType Nothing
    makeFace "Roboto Mono" bold normal "fonts/RobotoMono-Bold.ttf" TrueType Nothing
    makeFace "Roboto Mono" normal italic "fonts/RobotoMono-Italic.ttf" TrueType Nothing
    makeFace "Roboto Mono" normal normal "fonts/RobotoMono-Regular.ttf" TrueType Nothing
    makeFace "Roboto Slab" (weight 100) normal "fonts/RobotoSlab-Thin.ttf" TrueType Nothing
    makeFace "Roboto Slab" (weight 300) normal "fonts/RobotoSlab-Light.ttf" TrueType Nothing
    makeFace "Roboto Slab" bold normal "fonts/RobotoSlab-Bold.ttf" TrueType Nothing
    makeFace "Roboto Slab" normal normal "fonts/RobotoSlab-Regular.ttf" TrueType Nothing
    makeFace "fbb" normal normal "fonts/fbb-Regular.otf" OpenType Nothing
    makeFace "fbb" normal italic "fonts/fbb-Italic.otf" OpenType Nothing
    makeFace "fbb" bold normal "fonts/fbb-Bold.otf" OpenType Nothing
    makeFace "fbb" bold italic "fonts/fbb-BoldItalic.otf" OpenType Nothing
    makeFace "Font Awesome" normal normal "fonts/FontAwesome.otf" OpenType Nothing
    makeFace "STIX Two Math" normal normal "fonts/STIX2Math.otf" OpenType Nothing
  where
    makeFace fam weigh styl src_ typ muni = fontFace $ do
      fontFamily [fam] []
      fontWeight weigh
      fontStyle styl
      fontFaceSrc [FontFaceSrcUrl src_ (pure typ)]
      pure () `maybe` ("unicode-range" -:) $ muni

```
