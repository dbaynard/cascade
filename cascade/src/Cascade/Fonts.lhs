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
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Cascade.Fonts (
    module Cascade.Fonts
)   where

import "base" Data.Semigroup

import "clay" Clay

import Clay.Missing

makeMonospace = fontFamily ["Roboto Mono", "Noto Emoji", "Font Awesome"] [monospace]

makeDefaultFont = fontFamily ["Cardo", "Noto Emoji", "Font Awesome"] []

onlyFontAwesome = fontFamily ["Font Awesome"] []

modf = do
    makeMonospace
    "font-variant" -: "initial" -- TODO
    fontSize $ pct 80

emojiIcon icon = do
    makeDefaultFont
    color black
    contents [ stringContent icon
             ]

emojiIconBefore icon = before & do
    emojiIcon icon
    paddingRight $ em 0.3

emojiIconAfter icon = after & do
    emojiIcon icon
    paddingLeft $ em 0.3

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
        defFace gen src = fontFace $ do
            fontFamily [] [gen]
            --fontFaceSrc [FontFaceSrcLocal src] -- TODO
            "src" -: "prince-lookup(" <> src <> ")"

fonts :: Css
fonts = do
        makeFace "Cardo" bold normal "fonts/Cardo-Bold.ttf" TrueType
        makeFace "Cardo" normal italic "fonts/Cardo-Italic.ttf" TrueType
        makeFace "Cardo" normal normal "fonts/Cardo-Regular.ttf" TrueType
        makeFace "Noto Emoji" normal normal "fonts/NotoEmoji-Regular.ttf" TrueType
        makeFace "Roboto Mono" (weight 100) italic "fonts/RobotoMono-ThinItalic.ttf" TrueType
        makeFace "Roboto Mono" (weight 100) normal "fonts/RobotoMono-Thin.ttf" TrueType
        makeFace "Roboto Mono" (weight 300) italic "fonts/RobotoMono-LightItalic.ttf" TrueType
        makeFace "Roboto Mono" (weight 300) normal "fonts/RobotoMono-Light.ttf" TrueType
        makeFace "Roboto Mono" (weight 500) italic "fonts/RobotoMono-MediumItalic.ttf" TrueType
        makeFace "Roboto Mono" (weight 500) normal "fonts/RobotoMono-Medium.ttf" TrueType
        makeFace "Roboto Mono" bold italic "fonts/RobotoMono-BoldItalic.ttf" TrueType
        makeFace "Roboto Mono" bold normal "fonts/RobotoMono-Bold.ttf" TrueType
        makeFace "Roboto Mono" normal italic "fonts/RobotoMono-Italic.ttf" TrueType
        makeFace "Roboto Mono" normal normal "fonts/RobotoMono-Regular.ttf" TrueType
        makeFace "Roboto Slab" (weight 100) normal "fonts/RobotoSlab-Thin.ttf" TrueType
        makeFace "Roboto Slab" (weight 300) normal "fonts/RobotoSlab-Light.ttf" TrueType
        makeFace "Roboto Slab" bold normal "fonts/RobotoSlab-Bold.ttf" TrueType
        makeFace "Roboto Slab" normal normal "fonts/RobotoSlab-Regular.ttf" TrueType
        makeFace "fbb" normal normal "fonts/fbb-Regular.otf" OpenType
        makeFace "fbb" normal italic "fonts/fbb-Italic.otf" OpenType
        makeFace "fbb" bold normal "fonts/fbb-Bold.otf" OpenType
        makeFace "fbb" bold italic "fonts/fbb-BoldItalic.otf" OpenType
        makeFace "Font Awesome" normal normal "fonts/FontAwesome.otf" OpenType
    where
        makeFace fam weigh styl src typ = fontFace $ do
            fontFamily [fam] []
            fontWeight weigh
            fontStyle styl
            fontFaceSrc [FontFaceSrcUrl src (pure typ)]

```
