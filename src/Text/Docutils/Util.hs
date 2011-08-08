{-# LANGUAGE TypeOperators #-}

module Text.Docutils.Util where

import Text.XML.HXT.Core

import qualified Control.Category as C

type XmlT (~>) = XmlTree ~> XmlTree

doTransforms :: ArrowXml (~>) => [XmlT (~>)] -> XmlT (~>)
doTransforms ts = doAll (map processTopDown ts)

doAll :: ArrowXml (~>) => [XmlT (~>)] -> XmlT (~>)
doAll = foldr (>>>) C.id

onElem :: ArrowXml (~>) => String -> XmlT (~>) -> XmlT (~>)
onElem e trans = trans `when` isTag e

onElemA :: ArrowXml (~>) => String -> [(String, String)] -> XmlT (~>) -> XmlT (~>)
onElemA e attrs trans = trans `when` isTagA e attrs

isTag :: ArrowXml (~>) => String -> XmlT (~>)
isTag e = isElem >>> hasName e

hasAttrVal :: ArrowXml (~>) => String -> String -> XmlT (~>)
hasAttrVal a v = (hasAttr a >>> getAttrValue a >>> isA (== v)) `guards` C.id

isTagA :: ArrowXml (~>) => String -> [(String, String)] -> XmlT (~>)
isTagA e attrs = isTag e >>> doAll (map (uncurry hasAttrVal) attrs)

replaceTag :: ArrowXml (~>) => String -> String -> [XmlT (~>)] -> XmlT (~>)
replaceTag t1 t2 attrs = onElem t1 (setTag t2 attrs)

setTag :: ArrowXml (~>) => String -> [XmlT (~>)] -> XmlT (~>)
setTag t attrs = mkelem t attrs [getChildren]

mkLink :: ArrowXml (~>) => (XmlTree ~> String) -> XmlT (~>)
mkLink uri = mkelem "a"
             [ attr "href" (uri >>> mkText) ]
             [ C.id ]

