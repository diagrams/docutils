
module Text.Docutils.Transformers.Haskell where

import Text.Highlighting.Kate
import Text.XHtml.Strict

isHS :: ArrowXml (~>) => XmlTree ~> String
isHS = isElem >>>
       hasName "literal" >>> 
       hasAttr "classes" >>> getAttrValue "classes" >>> isA (== "hs")

highlightHS :: ArrowXml (~>) => XmlTree ~> XmlTree
highlightHS = getChildren >>> getText >>> arr f >>> selem "raw" [mkText]
  where f code = case highlightAs "haskell" code of
                   Left   _ -> code
                   Right ls -> showHtmlFragment (formatAsXHtml [OptInline] "haskell" ls)
