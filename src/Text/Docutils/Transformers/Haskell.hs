{-# LANGUAGE TypeOperators #-}

module Text.Docutils.Transformers.Haskell where

import Text.XML.HXT.Core

import Text.Highlighting.Kate
import Text.XHtml.Strict

import Text.Docutils.Util

hackagePrefix :: String
hackagePrefix = "http://hackage.haskell.org/package/"

hackage :: ArrowXml a => XmlT a
hackage = onElemA "literal" [("classes", "pkg")] $
            removeAttr "classes" >>>
            mkLink (getChildren >>> getText >>> arr (hackagePrefix ++))
            
isHS :: ArrowXml (~>) => XmlTree ~> String
isHS = isElem >>>
       hasName "literal" >>> 
       hasAttr "classes" >>> getAttrValue "classes" >>> isA (== "hs")

highlightHS :: ArrowXml (~>) => XmlTree ~> XmlTree
highlightHS = getChildren >>> getText >>> arr f >>> selem "raw" [mkText]
  where f code = case highlightAs "haskell" code of
                   Left   _ -> code
                   Right ls -> showHtmlFragment (formatAsXHtml [OptInline] "haskell" ls)
