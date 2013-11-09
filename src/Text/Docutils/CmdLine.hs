{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators      #-}

module Text.Docutils.CmdLine where

import           System.Exit

import           System.Console.CmdArgs
import           Text.XML.HXT.Core

data DocutilOpts = DocutilOpts
  { outputDir  :: FilePath
  , sourceFile :: FilePath
  , destFile   :: FilePath
  , keepGoing  :: Bool
  }
  deriving (Data, Typeable)

docutils :: DocutilOpts
docutils = DocutilOpts
  { outputDir    = def &= typDir &= help "Output directory for generated files."
  , sourceFile   = def &= argPos 0
  , destFile     = def &= argPos 1
  , keepGoing    = False &= help "Keep going on errors"
  }

docutilsCmdLine :: (FilePath -> IOSArrow XmlTree XmlTree) -> IO ExitCode
docutilsCmdLine transf = do
  opts <- cmdArgs docutils
  [rc] <- runX (application [withValidate no] opts transf)
  if rc >= c_err && not (keepGoing opts)
    then return (ExitFailure (0-1))
    else return ExitSuccess

application :: SysConfigList
            -> DocutilOpts
            -> (FilePath -> IOSArrow XmlTree XmlTree)
            -> IOSArrow b Int
application cfg opts transf
    = configSysVars cfg
      >>>
                     -- these options ensure it won't try to fetch the
                     -- DTD over the network
      readDocument [ withValidate no
                   , withSubstDTDEntities no
                   ] (sourceFile opts)
      >>>
      processChildren (transf (outputDir opts) `when` isElem)
      >>>
      writeDocument [withOutputXHTML] (destFile opts)
      >>>
      getErrStatus
