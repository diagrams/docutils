{-# LANGUAGE TypeOperators #-}

module Text.Docutils.CmdLine where

import System.Environment
import System.Exit

import Text.XML.HXT.Core

-- XXX generalize this
docutilsCmdLine :: IOSArrow XmlTree XmlTree -> IO ()
docutilsCmdLine transf = do
  argv <- getArgs
  (al, src, dst) <- cmdlineOpts argv
  [rc]  <- runX (application al src dst transf)
  if rc >= c_err
    then exitWith (ExitFailure (0-1))
    else exitWith ExitSuccess
 
-- XXX use CmdArgs
cmdlineOpts 	:: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = return ([withValidate no], argv!!0, argv!!1)
 
application	:: SysConfigList -> String -> String -> IOSArrow XmlTree XmlTree -> IOSArrow b Int
application cfg src dst transf
    = configSysVars cfg
      >>>
                     -- these options ensure it won't try to fetch the
                     -- DTD over the network
      readDocument [ withValidate no
                   , withSubstDTDEntities no
                   ] src
      >>>
      processChildren (transf `when` isElem)
      >>>
      writeDocument [withOutputXHTML] dst
      >>>
      getErrStatus

