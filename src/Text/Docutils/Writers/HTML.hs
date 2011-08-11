{-# LANGUAGE TypeOperators
           , Rank2Types
           , TypeSynonymInstances
           , NoMonomorphismRestriction 
  #-}

module Text.Docutils.Writers.HTML where

import Text.XML.HXT.Core

import Text.Docutils.Util

------------------------------------------------------------
--  The main XML -> HTML conversion
------------------------------------------------------------

xml2html :: ArrowXml (~>) => XmlT (~>)
xml2html = tSections >>>
           doTransforms 
           [ tDocument
           , tPara
           , tEmph
           , tBulletList
           , tListItem
             
           , tLiterals
             
           , tTitleRef
           , tReference
           ]

------------------------------------------------------------
--  Sections
------------------------------------------------------------

tSections :: ArrowXml (~>) => XmlT (~>)
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

tDocument :: ArrowXml (~>) => XmlT (~>)
tDocument = onElem "document" $ 
              selem "html" 
              [ selem "head" 
                [ selem "title" [getChildren >>> hasName "title" >>> getChildren]
                ]
              , selem "body"
                [ mkelem "div"
                  [ attr "class" (txt "document") ]
                  [ getChildren >>>
                    ( onElem "title" $
                      mkelem "h1"
                      [ attr "class" (txt "title") ]
                      [ getChildren ]
                    )
                  ]
                ]
              ]  

tLiterals :: ArrowXml (~>) => XmlT (~>)
tLiterals = replaceTag "literal" "code" []
        
tPara :: ArrowXml (~>) => XmlT (~>)
tPara = replaceTag "paragraph" "p" []

tTitleRef :: ArrowXml (~>) => XmlT (~>)
tTitleRef = replaceTag "title_reference" "cite" []

-- XXX check whether it is an internal or other sort of reference
-- based on the attributes.
tReference :: ArrowXml (~>) => XmlT (~>)
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

tEmph :: ArrowXml (~>) => XmlT (~>)
tEmph = replaceTag "emphasis" "em" []

tBulletList :: ArrowXml (~>) => XmlT (~>)
tBulletList = replaceTag "bullet_list" "ul" []

tListItem :: ArrowXml (~>) => XmlT (~>)
tListItem = replaceTag "list_item" "li" []