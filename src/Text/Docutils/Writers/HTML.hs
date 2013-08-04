{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Text.Docutils.Writers.HTML where

import qualified Control.Category   as C
import           Text.XML.HXT.Core

import           Text.Docutils.Util

------------------------------------------------------------
--  The main XML -> HTML conversion
------------------------------------------------------------

xml2html :: ArrowXml a => XmlT a
xml2html = tSections >>>
           doTransforms
           [ tDocument
           , tTitle
           , tTopic
           , tDispMath
           , tPara
           , tEmph
           , tImage
           , tBulletList
           , tEnumList
           , tListItem
           , tBlockquote
           , tMath
           , tContainer

           , tLiterals
           , tLiteralBlocks

           , tTitleRef
           , tReference
           , tTarget

           , tEmDash
           ]

------------------------------------------------------------
--  Sections
------------------------------------------------------------

tSections :: ArrowXml a => XmlT a
tSections = tSections' (1 :: Integer)
  where tSections' n       = titleSection n `orElse` processChildren (tSections' n)
        titleSection n     = isTag "section" >>>
                             mkelem "div"
                               [ attr "class" (txt "section")
                               , attr "id" (getAttrValue "ids" >>> mkText)
                               ]
                               [getChildren >>>
                                  ( (isTag "title" >>> setTag ("h" ++ show n) []) `orElse`
                                    tSections' (n+1)
                                  )
                               ]

------------------------------------------------------------
--  Atomic transformations
------------------------------------------------------------

-- XXX need to generalize this to allow generating both standalone and
-- fragments

tDocument :: ArrowXml a => XmlT a
tDocument = onElem "document" $
                mkelem "div"
                [ attr "class" (txt "document") ]
                [ getChildren >>>
                  ( onElem "title" $
                    mkelem "h1"
                    [ attr "class" (txt "title") ]
                    [ getChildren ]
                  )
                ]

-- replace any leftover title tags
tTitle :: ArrowXml a => XmlT a
tTitle = replaceTag "title" "h2" []

tTopic :: ArrowXml a => XmlT a
tTopic = onElem "topic" $
  eelem "div"
    += attr "class" (getAttrValue "classes" >>> mkText)
    += attr "id"    (getAttrValue "ids" >>> mkText)
    += getChildren

tLiterals :: ArrowXml a => XmlT a
tLiterals = replaceTag "literal" "code" []

tLiteralBlocks :: ArrowXml a => XmlT a
tLiteralBlocks = onElem "literal_block" $
  eelem "pre"
    += (eelem "code" += getChildren)

tPara :: ArrowXml a => XmlT a
tPara = replaceTag "paragraph" "p" []

tTitleRef :: ArrowXml a => XmlT a
tTitleRef = replaceTag "title_reference" "cite" []

-- XXX check whether it is an internal or other sort of reference
-- based on the attributes.
tReference :: ArrowXml a => XmlT a
tReference = onElem "reference" $
  ifA (hasAttr "refuri")
    (setTag "a"
     [ attr "class" (txt "reference external")
     , attr "href" (getAttrValue "refuri" >>> mkText)
     ]
    )
    (setTag "a"
     [ attr "class" (txt "reference internal")
     , attr "href" (getAttrValue "refid" >>> arr ('#':) >>> mkText)
     ]
    )

tTarget :: ArrowXml a => XmlT a
tTarget = onElem "target" $ txt ""

tEmph :: ArrowXml a => XmlT a
tEmph = replaceTag "emphasis" "em" []

tImage :: ArrowXml a => XmlT a
tImage = onElem "image" $
  eelem "div"
    += attr "style" (getAttrValue "align" >>> arr ("text-align: " ++) >>> mkText)
    += (eelem "img"
          += attr "src" (getAttrValue "uri" >>> mkText)
          += getAttrl
       )

tBulletList :: ArrowXml a => XmlT a
tBulletList = replaceTag "bullet_list" "ul" []

tEnumList :: ArrowXml a => XmlT a
tEnumList = replaceTag "enumerated_list" "ol" []

tListItem :: ArrowXml a => XmlT a
tListItem = replaceTag "list_item" "li" []

tBlockquote :: ArrowXml a => XmlT a
tBlockquote = replaceTag "block_quote" "blockquote" []

-- XXX fix me
--   2. merge with previous and next paragraphs if present
tDispMath :: ArrowXml a => XmlT a
tDispMath = onElem "paragraph" $
  (getChildren >>> getChildren >>> getText >>> arr (("\\[" ++) . (++ "\\]")) >>> mkText)
  `when`
  (listA getChildren >>> isA ((==1) . length) >>> unlistA
    >>> isElem >>> hasName "math")

tMath :: ArrowXml a => XmlT a
tMath = onElem "math" $
  getChildren >>> getText >>> arr (("\\(" ++) . (++ "\\)")) >>> mkText

tContainer :: ArrowXml a => XmlT a
tContainer = onElem "container" $
  eelem "div"
    += attr "class" (getAttrValue "classes" >>> mkText)
    += getChildren

tEmDash :: ArrowXml a => XmlT a
tEmDash = (getText >>> arr mkEmDashes >>> mkText) `when` isText
  where mkEmDashes [] = []
        mkEmDashes ('-':'-':'-':t) = '—' : mkEmDashes t
        mkEmDashes (c:t)           = c   : mkEmDashes t

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

styleFile :: ArrowXml a => String -> XmlT a
styleFile s =
  onElem "head" $
    C.id += (eelem "link" += attr "rel" (txt "stylesheet")
                          += attr "type" (txt "text/css")
                          += attr "href" (txt s)
            )
