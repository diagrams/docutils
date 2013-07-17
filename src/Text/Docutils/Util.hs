{-# LANGUAGE TypeOperators #-}

module Text.Docutils.Util where

import           Text.XML.HXT.Core

import qualified Control.Category  as C

type XmlT a = XmlTree `a` XmlTree

doTransforms :: ArrowXml a => [XmlT a] -> XmlT a
doTransforms ts = doAll (map processTopDown ts)

doAll :: ArrowXml a => [XmlT a] -> XmlT a
doAll = foldr (>>>) C.id

onElem :: ArrowXml a => String -> XmlT a -> XmlT a
onElem e trans = trans `when` isTag e

onElemA :: ArrowXml a => String -> [(String, String)] -> XmlT a -> XmlT a
onElemA e attrs trans = trans `when` isTagA e attrs

isTag :: ArrowXml a => String -> XmlT a
isTag e = isElem >>> hasName e

hasAttrVal :: ArrowXml a => String -> String -> XmlT a
hasAttrVal a v = (hasAttr a >>> getAttrValue a >>> isA (== v)) `guards` C.id

isTagA :: ArrowXml a => String -> [(String, String)] -> XmlT a
isTagA e attrs = isTag e >>> doAll (map (uncurry hasAttrVal) attrs)

replaceTag :: ArrowXml a => String -> String -> [XmlT a] -> XmlT a
replaceTag t1 t2 attrs = onElem t1 (setTag t2 attrs)

setTag :: ArrowXml a => String -> [XmlT a] -> XmlT a
setTag t attrs = mkelem t attrs [getChildren]

mkLink :: ArrowXml a => (XmlTree `a` String) -> XmlT a
mkLink uri = mkelem "a"
             [ attr "href" (uri >>> mkText) ]
             [ C.id ]

