{-# LANGUAGE TypeOperators #-}

module Text.Docutils.Transformers.Haskell where

import Text.XML.HXT.Core

import Text.Highlighting.Kate
import Text.XHtml.Strict

import Text.Docutils.Util

hackagePrefix :: String
hackagePrefix = "http://hackage.haskell.org/package/"

hackage :: ArrowXml a => XmlT a
hackage = 
  onElemA "literal" [("classes", "pkg")] $
    removeAttr "classes" >>>
    mkLink (getChildren >>> getText >>> arr (hackagePrefix ++))
            
highlightHS :: ArrowXml (~>) => XmlTree ~> XmlTree
highlightHS = 
  onElemA "literal" [("classes", "hs")] $
    removeAttr "classes" >>>
    getChildren >>> getText >>> arr highlight >>> hread
  where highlight code = case highlightAs "haskell" code of
                           Left   _ -> code
                           Right ls -> showHtmlFragment (formatAsXHtml [OptInline] "haskell" ls)
