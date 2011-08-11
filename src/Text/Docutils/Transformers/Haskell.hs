{-# LANGUAGE TypeOperators #-}

module Text.Docutils.Transformers.Haskell where

import GHC
import Packages
import DynFlags
import MonadUtils
import Module
import GHC.Paths (libdir)

import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as M

import Data.List.Split

import qualified Control.Category as C
import Control.Arrow

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

linkifyModules :: ArrowXml (~>) => ModuleMap -> XmlT (~>)
linkifyModules modMap =
  onElemA "literal" [("classes", "mod")] $
    removeAttr "classes" >>>
    mkLink (getChildren >>> getText >>> arr (mkAPILink modMap))

mkAPILink :: ModuleMap -> String -> String
mkAPILink modMap modName = hackageAPIPrefix ++ pkg ++ hackageAPIPath ++ modPath
  where modPath = map f modName ++ ".html"
        f '.' = '-'
        f x   = x
        pkg   = packageIdStringBase . fromJust $ M.lookup (mkModuleName modName) modMap
        -- XXX

------------------------------------------------------------
--  Packages + modules
------------------------------------------------------------

type ModuleMap = M.Map ModuleName PackageId

-- | Convert a 'PackageId' to a String without the trailing version number.
packageIdStringBase :: PackageId -> String
packageIdStringBase = intercalate "-" . init . splitOn "-" . packageIdString

-- | Get the list of modules provided by a package.
getPkgModules :: String -> IO (Maybe (PackageId, [ModuleName]))
getPkgModules pkg =
  defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = dflags { packageFlags = ExposePackage pkg : packageFlags dflags }
      (dflags'', pids) <- liftIO $ initPackages dflags'
      let pkgSt   = pkgState dflags''
          mpid    = listToMaybe (filter ((pkg `isPrefixOf`) . packageIdString) pids)
      return $ (id &&& (exposedModules . getPackageDetails pkgSt)) `fmap` mpid

-- | Given a list of package names, build a mapping from module names to
--   packages so we can look up what package provides a given module.
buildModuleMap :: [String] -> IO ModuleMap
buildModuleMap pkgs = do
  pkgMods <- catMaybes `fmap` mapM getPkgModules pkgs
  return . M.fromList . map swap . concat . map strength $ pkgMods
 where strength (x,f) = fmap ((,) x) f

-- To do:
--   + linkify highlighted code (link to APIs)
--   + automatically look up/insert type signatures
--   + automatically typeset ghci sessions

--   + automatically render embedded diagrams!