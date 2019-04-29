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
makeSerifFont = fontFamily ["Cardo", "Noto Emoji", "EB Garamond", "Font Awesome", "STIX Two Math"] []

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

    makeFace "EB Garamond" normal normal "fonts/EBGaramond-Regular.ttf" TrueType Nothing
    makeFace "EB Garamond" normal italic "fonts/EBGaramond-Italic.ttf" TrueType Nothing
    makeFace "EB Garamond" (weight 500) normal "fonts/EBGaramond-Medium.ttf" TrueType Nothing
    makeFace "EB Garamond" (weight 500) italic "fonts/EBGaramond-MediumItalic.ttf" TrueType Nothing
    makeFace "EB Garamond" (weight 600) normal "fonts/EBGaramond-SemiBold.ttf" TrueType Nothing
    makeFace "EB Garamond" (weight 600) italic "fonts/EBGaramond-SemiBoldItalic.ttf" TrueType Nothing
    makeFace "EB Garamond" bold normal "fonts/EBGaramond-Bold.ttf" TrueType Nothing
    makeFace "EB Garamond" bold italic "fonts/EBGaramond-BoldItalic.ttf" TrueType Nothing
    makeFace "EB Garamond" (weight 800) normal "fonts/EBGaramond-ExtraBold.ttf" TrueType Nothing
    makeFace "EB Garamond" (weight 800) italic "fonts/EBGaramond-ExtraBoldItalic.ttf" TrueType Nothing

    makeFace "Noto Serif" normal normal "fonts/NotoSerif-Regular.ttf" TrueType Nothing
    makeFace "Noto Serif" normal italic "fonts/NotoSerif-Italic.ttf" TrueType Nothing
    makeFace "Noto Serif" bold normal "fonts/NotoSerif-Bold.ttf" TrueType Nothing
    makeFace "Noto Serif" bold italic "fonts/NotoSerif-BoldItalic.ttf" TrueType Nothing

    makeFace "Linux Biolinum" normal normal "fonts/LinBiolinum_R.otf" OpenType Nothing
    makeFace "Linux Biolinum" normal italic "fonts/LinBiolinum_RI.otf" OpenType Nothing
    makeFace "Linux Biolinum" bold normal "fonts/LinBiolinum_RB.otf" OpenType Nothing
    makeFace "Linux Biolinum Keyboard" normal normal "fonts/LinBiolinum_K.otf" OpenType Nothing

    makeFace "Linux Libertine" normal normal "fonts/LinLibertine_R.otf" OpenType Nothing
    makeFace "Linux Libertine" normal italic "fonts/LinLibertine_RI.otf" OpenType Nothing
    makeFace "Linux Libertine" (weight 500) normal "fonts/LinLibertine_RZ.otf" OpenType Nothing
    makeFace "Linux Libertine" (weight 500) italic "fonts/LinLibertine_RZI.otf" OpenType Nothing
    makeFace "Linux Libertine" bold normal "fonts/LinLibertine_RB.otf" OpenType Nothing
    makeFace "Linux Libertine" bold italic "fonts/LinLibertine_RBI.otf" OpenType Nothing
    makeFace "Linux Libertine Mono" normal normal "fonts/LinLibertine_M.otf" OpenType Nothing
    makeFace "Linux Libertine Display" normal normal "fonts/LinLibertine_DR.otf" OpenType Nothing
    makeFace "Linux Libertine Initials" normal normal "fonts/LinLibertine_I.otf" OpenType Nothing

    makeFace "Inconsolata" normal normal "fonts/Inconsolata-Regular.ttf" TrueType Nothing
    makeFace "Inconsolata" bold normal "fonts/Inconsolata-Bold.ttf" TrueType Nothing

  where
    makeFace fam weigh styl src_ typ muni = fontFace $ do
      fontFamily [fam] []
      fontWeight weigh
      fontStyle styl
      fontFaceSrc [FontFaceSrcUrl src_ (pure typ)]
      pure () `maybe` ("unicode-range" -:) $ muni

```
