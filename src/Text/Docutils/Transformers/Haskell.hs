{-# LANGUAGE TypeOperators #-}

module Text.Docutils.Transformers.Haskell where

import qualified Control.Category as C

import Text.XML.HXT.Core

import Text.Highlighting.Kate
import Text.XHtml.Strict

import Text.Docutils.Util

hackagePkgPrefix :: String
hackagePkgPrefix = "http://hackage.haskell.org/package/"

hackageAPIPrefix, hackageAPIPath :: String
hackageAPIPrefix = "http://hackage.haskell.org/packages/archive/"
hackageAPIPath   = "/latest/doc/html/"

linkifyHackage :: ArrowXml a => XmlT a
linkifyHackage = 
  onElemA "literal" [("classes", "pkg")] $
    removeAttr "classes" >>>
    mkLink (getChildren >>> getText >>> arr (hackagePkgPrefix ++))
            
highlightHS :: ArrowXml (~>) => XmlTree ~> XmlTree
highlightHS = 
  onElemA "literal" [("classes", "hs")] $
    removeAttr "classes" >>>
    getChildren >>> getText >>> arr highlight >>> hread
  where highlight code = case highlightAs "haskell" code of
                           Left   _ -> code
                           Right ls -> showHtmlFragment (formatAsXHtml [OptInline] "haskell" ls)

styleFile :: ArrowXml (~>) => String -> XmlT (~>)
styleFile s = 
  onElem "head" $
    C.id += (eelem "link" += attr "rel" (txt "stylesheet")
                          += attr "type" (txt "text/css")
                          += attr "href" (txt s)
            )

-- XXX option to give multiple packages and have it look up the
-- contents to determine where to link

-- first argument is package name
linkifyModules :: ArrowXml (~>) => String -> XmlT (~>)
linkifyModules pkgName =
  onElemA "literal" [("classes", "mod")] $
    removeAttr "classes" >>>
    mkLink (getChildren >>> getText >>> arr (mkAPILink pkgName))

mkAPILink :: String -> String -> String
mkAPILink pkgName modName = hackageAPIPrefix ++ pkgName ++ hackageAPIPath ++ modPath
  where modPath = map f modName ++ ".html"
        f '.' = '-'
        f x   = x
