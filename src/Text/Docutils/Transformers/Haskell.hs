{-# LANGUAGE TypeOperators
           , PatternGuards
           , Arrows
  #-}

module Text.Docutils.Transformers.Haskell where

-- import Debug.Trace

import GHC
import Name
import Packages
import DynFlags
import MonadUtils
import Module
import GHC.Paths (libdir)

import Data.Char
import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as M

import Data.List.Split

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
    eelem "span"
      += attr "class" (txt "package")
      += mkLink (getChildren >>> getText >>> arr (hackagePkgPrefix ++))

highlightInlineHS :: ArrowXml (~>) => XmlT (~>)
highlightInlineHS =
  onElemA "literal" [("classes", "hs")] $
    removeAttr "classes" >>>
    getChildren >>> getText >>> arr (highlightHS [OptInline]) >>> hread

highlightBlockHS :: ArrowXml (~>) => XmlT (~>)
highlightBlockHS =
  onElemA "literal_block" [("classes", "lhs")] $
    eelem "div"
      += attr "class" (txt "examplesrc")
      += highlightBlockHSArr

-- XXX todo:
--   - For things exported by multiple modules find the right one? E.g. right now
--     atop is exported by Diagrams.Prelude but its documentation is not there
--     since it is exported via a whole module

--   - splitting on whitespace is not actually a good method of
--     identifying things to link.  Really ought to use a proper
--     parser.  The problem is that there can be markup in there
--     already from the syntax highlighter.
linkifyHS :: (ArrowChoice (~>), ArrowXml (~>)) => NameMap -> ModuleMap -> XmlT (~>)
linkifyHS nameMap modMap = onElemA "code" [("class", "sourceCode LiterateHaskell")] $
                             linkifyHS'
  where linkifyHS' = (isText >>> linkifyAll) `orElse` (processChildren linkifyHS')
        linkifyAll = getText
                 >>> arrL (split (condense $ oneOf " "))
                 >>> linkify
        linkify    = proc t -> do
                       case M.lookup (stripSpecials t) nameMap of
                         Nothing   -> mkText -< t
                         Just modN ->
                           mkText >>>
                           mkLink
                             (constA
                               (mkAPILink modMap (Just (encode (stripSpecials t)))
                                 (moduleNameString modN)
                               )
                             )              -<< t
        stripSpecials ""       = ""
        stripSpecials "("      = ""
        stripSpecials "`"      = ""
        stripSpecials ('(':t') = init t'
        stripSpecials ('`':t') = init t'
        stripSpecials t' = t'
        encode = concatMap encodeC
        encodeC c | isAlphaNum c = [c]
                  | otherwise    = '-' : show (ord c) ++ "-"

highlightBlockHSArr :: ArrowXml (~>) => XmlT (~>)
highlightBlockHSArr =
  getChildren >>> getText >>> arr (litify >>> highlightHS []) >>> hread


highlightHS :: [FormatOption] -> String -> String
highlightHS opts code =
  case highlightAs "LiterateHaskell" code of
    Left   _ -> code
    Right ls -> showHtmlFragment (formatAsXHtml opts "LiterateHaskell" ls)

-- | If any lines begin with "> ", assume it is literate Haskell and
--   leave it alone.  Otherwise, prefix every line with "> ".
litify :: String -> String
litify code | any ("> " `isPrefixOf`) ls = code
            | otherwise = unlines . map ("> " ++) $ ls
  where ls = lines code

linkifyModules :: ArrowXml (~>) => ModuleMap -> XmlT (~>)
linkifyModules modMap =
  onElemA "literal" [("classes", "mod")] $
    removeAttr "classes" >>>
    eelem "span"
      += attr "class" (txt "module")
      += mkLink (getChildren >>> getText >>> arr (mkAPILink modMap Nothing))

-- XXX generalize this...

mkAPILink :: ModuleMap -> Maybe String -> String -> String
mkAPILink modMap mexp modName
--  = hackageAPIPrefix ++ pkg ++ hackageAPIPath ++ modPath ++ expHash
  = "/doc/" ++ modPath ++ expHash   -- for linking to local API reference
  where modPath = map f modName ++ ".html"
        f '.' = '-'
        f x   = x
        pkg   = packageIdStringBase . fromJust {- . traceShow modName -} $ M.lookup (mkModuleName modName) modMap
        -- XXX fix me!!!  Should not use fromJust, rather insert some
        -- kind of error marker if the module is not found.  Need to
        -- pull this processing out of mkAPILink since at this point it is too late.
        -- See implementation of linkifyHS for inspiration.
        expHash | Just e@(e1:_) <- mexp = case () of
                    _ | isUpper e1 -> "#t:" ++ e
                      | otherwise  -> "#v:" ++ e
                | otherwise      = ""

------------------------------------------------------------
--  Packages + modules
------------------------------------------------------------

-- | A mapping from modules to packages.
type ModuleMap = M.Map ModuleName PackageId

-- | A mapping from exported names to modules.
type NameMap = M.Map String ModuleName

-- | Convert a 'PackageId' to a String without the trailing version number.
packageIdStringBase :: PackageId -> String
packageIdStringBase = intercalate "-" . init . splitOn "-" . packageIdString

-- | Get the list of modules provided by a package.
getPkgModules :: String -> IO (Maybe (PackageId, [(ModuleName, ModuleInfo)]))
getPkgModules pkg =
  defaultErrorHandler defaultLogAction $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = dflags { packageFlags = ExposePackage pkg : packageFlags dflags }
      (dflags'', pids) <- liftIO $ initPackages dflags'
      _ <- setSessionDynFlags dflags''
      let pkgSt    = pkgState dflags''
          mpid     = listToMaybe (filter ((pkg `isPrefixOf`) . packageIdString) pids)
          mpkgMods = (id &&& (exposedModules . getPackageDetails pkgSt)) <$> mpid
      case mpkgMods of
        Nothing          -> return Nothing
        Just (pkgid, ns) -> do
          mis <- catMaybes <$>
                   mapM (fmap strength . strength . (id &&& getModuleInfo . mkModule pkgid)) ns
          return . Just $ (pkgid, mis)

-- | Given a list of package names, build a mapping from module names to
--   packages so we can look up what package provides a given module.
buildPackageMaps :: [String] -> IO (ModuleMap, NameMap)
buildPackageMaps pkgs = do
  pkgMods <- catMaybes <$> mapM getPkgModules pkgs
  let pkgModPairs = concat . map strength $ pkgMods
      modMap      = M.fromList . map (first fst . swap) $ pkgModPairs
      nameMap     = M.fromList
                    . (map . first) (occNameString . nameOccName)
                    . concatMap (map swap . strength . second modInfoExports . snd)
                    $ pkgModPairs
  return (modMap, nameMap)

strength :: Functor f => (a, f b) -> f (a, b)
strength (x,f) = fmap ((,) x) f


-- To do:
--   + automatically look up/insert type signatures
--   + automatically typeset ghci sessions
