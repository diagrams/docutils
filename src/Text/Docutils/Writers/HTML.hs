{-# LANGUAGE TypeOperators
           , Rank2Types
           , TypeSynonymInstances
           , NoMonomorphismRestriction 
  #-}

module Text.Docutils.Writers.HTML where

import Text.XML.HXT.Core

import qualified Control.Category as C

type XmlT (~>) = XmlTree ~> XmlTree

------------------------------------------------------------
--  The main XML -> HTML conversion
------------------------------------------------------------

xmlToHtml :: ArrowXml (~>) => XmlT (~>)
xmlToHtml = tSections >>>
            doTransforms [ tDocument
                         , tLiterals
                         , tRaw
                         , tPara
                         , tTitleRef
                         , tInternalRef
                         ]

------------------------------------------------------------
--  Combinators
------------------------------------------------------------

doTransforms :: ArrowXml (~>) => [XmlT (~>)] -> XmlT (~>)
doTransforms ts = doAll (map processTopDown ts)

doAll :: ArrowXml (~>) => [XmlT (~>)] -> XmlT (~>)
doAll = foldr (>>>) C.id

onElem :: ArrowXml (~>) => String -> XmlT (~>) -> XmlT (~>)
onElem e trans = trans `when` isTag e

isTag :: ArrowXml (~>) => String -> XmlT (~>)
isTag e = isElem >>> hasName e

replaceTag :: ArrowXml (~>) => String -> String -> XmlT (~>)
replaceTag t1 t2 = onElem t1 (setTag t2)

setTag :: ArrowXml (~>) => String -> XmlT (~>)
setTag t = selem t [getChildren]

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
                                  ( (isTag "title" >>> setTag ("h" ++ show n)) `orElse`
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
tLiterals = replaceTag "literal" "code"
        
tRaw :: ArrowXml (~>) => XmlT (~>)
tRaw = onElem "raw" $ getChildren

tPara :: ArrowXml (~>) => XmlT (~>)
tPara = replaceTag "paragraph" "p"

tTitleRef :: ArrowXml (~>) => XmlT (~>)
tTitleRef = replaceTag "title_reference" "cite"

tInternalRef :: ArrowXml (~>) => XmlT (~>)
tInternalRef = onElem "reference" $
  mkelem "a"
  [ attr "class" (txt "reference internal")
  , attr "href" (getAttrValue "refid" >>> arr ('#':) >>> mkText)
  ]
  [ getChildren ]