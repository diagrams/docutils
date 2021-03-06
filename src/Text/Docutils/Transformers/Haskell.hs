{-# LANGUAGE Arrows        #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Text.Docutils.Transformers.Haskell where

-- import Debug.Trace

-- import           DynFlags                           (ModRenaming (..),
--                                                      PackageArg (PackageArg),
--                                                      PackageFlag (ExposePackage),
--                                                      defaultFatalMessager,
--                                                      defaultFlushOut)

-- import           GHC                                (ModuleInfo,
--                                                      defaultErrorHandler,
--                                                      getModuleInfo,
--                                                      getSessionDynFlags,
--                                                      modInfoExports, noLoc,
--                                                      packageFlags,
--                                                      parseDynamicFlags,
--                                                      pkgState, runGhc,
--                                                      setSessionDynFlags)

-- import           GHC.PackageDb                      (exposedName)
-- import           GHC.Paths                          (libdir)

  -- XXX NB: in GHC 8.0.1 PackageKey is renamed to UnitId, and now
  -- contains a hash instead of a package name.  We will need to come
  -- up with a different way to figure out the package name.
-- import           Module                             (ModuleName, PackageKey,
--                                                      mkModule, mkModuleName,
--                                                      moduleNameString,
--                                                      packageKeyString)
-- import           MonadUtils                         (liftIO)
-- import           Name                               (nameOccName, occNameString)
-- import           Packages                           (exposedModules,
--                                                      getPackageDetails,
--                                                      initPackages)

import           Control.Applicative                ((<$>))
import           Data.Char
import           Data.Function                      (on)
import           Data.List                          (intercalate, isPrefixOf,
                                                     sortBy)
import qualified Data.Map                           as M
import           Data.Maybe                         (catMaybes, fromMaybe,
                                                     listToMaybe)

import           Data.List.Split                    (condense, oneOf, split,
                                                     splitOn)

import qualified Text.XML.HXT.Arrow.ParserInterface as PI
import           Text.XML.HXT.Core

import           Text.Blaze.Html                    (Html)
import           Text.Blaze.Html.Renderer.String    (renderHtml)
import           Text.Highlighting.Kate             (defaultFormatOpts,
                                                     formatHtmlBlock,
                                                     formatHtmlInline,
                                                     highlightAs)
import           Text.Highlighting.Kate.Types       (SourceLine)

import           System.Environment                 (getEnvironment)
import           Text.Docutils.Util                 (XmlT, mkLink, onElemA)

-- HXT 9.3.1.7 changed hread to canonicalize values dropping some content
-- this hread' gives us the old behavior.
hread' :: ArrowXml a => a String XmlTree
hread' = fromLA $ PI.hread >>> editNTreeA [isError :-> none]


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

highlightInlineHS :: ArrowXml a => XmlT a
highlightInlineHS =
  onElemA "literal" [("classes", "hs")] $
    removeAttr "classes" >>>
    getChildren >>> getText >>> arr highlightHSInline >>> hread'

highlightBlockHS :: ArrowXml a => XmlT a
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
-- linkifyHS :: (ArrowChoice a, ArrowXml a)
--           => (String -> String -> Ordering)  -- earlier modules are preferred
--           -> NameMap -> ModuleMap -> XmlT a
-- linkifyHS modComp nameMap modMap = onElemA "code" [("class", "sourceCode")] $
--                              linkifyHS'
--   where linkifyHS' = (isText >>> linkifyAll) `orElse` (processChildren linkifyHS')
--         linkifyAll = getText
--                  >>> arrL (split (condense $ oneOf " "))
--                  >>> linkify
--         linkify    = proc t -> do
--                        case M.lookup (stripSpecials t) nameMap of
--                          Nothing    -> mkText -< t
--                          Just []    -> mkText -< t
--                          Just modNs ->
--                            mkText >>>
--                            mkLink
--                              (constA
--                                (mkAPILink modMap (Just (encode (stripSpecials t)))
--                                  (moduleNameString (head $ sortBy (modComp `on` moduleNameString) modNs))
--                                )
--                              )              -<< t
--         stripSpecials ""       = ""
--         stripSpecials "("      = ""
--         stripSpecials "`"      = ""
--         stripSpecials ('(':t') = init t'
--         stripSpecials ('`':t') = init t'
--         stripSpecials t' = t'
--         encode = concatMap encodeC
--         encodeC c | isAlphaNum c = [c]
--                   | otherwise    = '-' : show (ord c) ++ "-"

highlightBlockHSArr :: ArrowXml a => XmlT a
highlightBlockHSArr =
  getChildren >>> getText >>> arr (litify >>> highlightHSBlock) >>> hread'

highlightHSInline, highlightHSBlock :: String -> String
highlightHSInline = highlightHS defaultFormatOpts formatHtmlInline "Haskell"
highlightHSBlock  = highlightHS defaultFormatOpts formatHtmlBlock  "LiterateHaskell"

highlightHS :: opts -> (opts -> [SourceLine] -> Html) -> String -> String -> String
highlightHS opts fmt as =
  renderHtml . fmt opts . highlightAs as

-- | If any lines begin with "> ", assume it is literate Haskell and
--   leave it alone.  Otherwise, prefix every line with "> ".
litify :: String -> String
litify code | any ("> " `isPrefixOf`) ls = code
            | otherwise = unlines . map ("> " ++) $ ls
  where ls = lines code

-- linkifyModules :: ArrowXml a => ModuleMap -> XmlT a
-- linkifyModules modMap =
--   onElemA "literal" [("classes", "mod")] $
--     removeAttr "classes" >>>
--     eelem "span"
--       += attr "class" (txt "module")
--       += mkLink (getChildren >>> getText >>> arr (mkAPILink modMap Nothing))

-- XXX generalize this...

-- mkAPILink :: ModuleMap -> Maybe String -> String -> String
-- mkAPILink modMap mexp modName
-- --  = hackageAPIPrefix ++ pkg ++ hackageAPIPath ++ modPath ++ expHash
--   = "/haddock/" ++ pkgDir ++ modPath ++ expHash   -- for linking to local API reference
--   where modPath = map f modName ++ ".html"
--         f '.'  = '-'
--         f x    = x
--         pkgDir = maybe "" (++"/") $ M.lookup (mkModuleName modName) modMap
--         expHash | Just e@(e1:_) <- mexp = case () of
--                     _ | isUpper e1 -> "#t:" ++ e
--                       | otherwise  -> "#v:" ++ e
--                 | otherwise      = ""

------------------------------------------------------------
--  Packages + modules
------------------------------------------------------------

{-

-- | A mapping from modules to package names.
type ModuleMap = M.Map ModuleName String

-- | A mapping from exported names to modules.  Since multiple modules
--   may re-export the same name, we map from names to a list of
--   modules.
type NameMap = M.Map String [ModuleName]

getHsenvArgv :: IO [String]
getHsenvArgv = do
  env <- getEnvironment
  return $ case (lookup "HSENV" env) of
             Nothing -> []
             _       -> hsenvArgv
                 where hsenvArgv = words $ fromMaybe "" (lookup "PACKAGE_DB_FOR_GHC" env)

-- | Get the list of modules provided by a package.
getPkgModules :: String -> IO (Maybe [(ModuleName, ModuleInfo)])
getPkgModules pkg =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      let dflags1 = dflags0 { packageFlags = ExposePackage (PackageArg pkg) (ModRenaming False []) : packageFlags dflags0 }
      args <- liftIO getHsenvArgv
      let args' = map noLoc args
      (dflags2, _, _) <- parseDynamicFlags dflags1 args'
      (dflags3, pids) <- liftIO $ initPackages dflags2
      -- pids :: [PackageKey]
      _ <- setSessionDynFlags dflags3
      let mpid     = listToMaybe (filter ((take 5 pkg `isPrefixOf`) . (take 5 . packageKeyString)) pids)
      let mpkgMods = (id &&& (map exposedName . exposedModules . getPackageDetails dflags3)) <$> mpid
      case mpkgMods of
        Nothing          -> return Nothing
        Just (pkgid, ns) -> do
          mis <- catMaybes <$>
                   mapM (fmap strength . strength . (id &&& getModuleInfo . mkModule pkgid)) ns
          return . Just $ mis

-- | Given a list of package names, build two mappings: one from
--   module names to packages so we can look up what package provides
--   a given module, and one from identifier names to module names so
--   we can look up what module exports a given identifier name.
buildPackageMaps :: [String] -> IO (ModuleMap, NameMap)
buildPackageMaps pkgs = do

  pkgMods <- catMaybes <$> mapM (\pkg -> fmap (pkg,) <$> getPkgModules pkg) pkgs

  -- concat . map strength $ pkgMods :: [(String, (ModuleName, ModuleInfo))]
  let pkgModPairs = concat . map strength $ pkgMods

      -- [(ModuleName, String)]
      modMap      = M.fromList . map (first fst . swap) $ pkgModPairs

      nameMap     = buildMultiMap
                    . (map . first) (occNameString . nameOccName)
                    . concatMap (map swap . strength . second modInfoExports . snd)
                    $ pkgModPairs
      buildMultiMap :: Ord k => [(k,a)] -> M.Map k [a]
      buildMultiMap = foldr (uncurry (M.insertWith (++))) M.empty . (map . second) (:[])

  return (modMap, nameMap)

strength :: Functor f => (a, f b) -> f (a, b)
strength (x,f) = fmap ((,) x) f


-- To do:
--   + automatically look up/insert type signatures
--   + automatically typeset ghci sessions

-}
