{-# LANGUAGE TypeOperators #-}

module Text.Docutils.CmdLine where

import System.Environment
import System.Exit

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP

import Text.Docutils.Writers.HTML

-- XXX generalize this
docutilsCmdLine :: IO ()
docutilsCmdLine = do
  argv <- getArgs
  (al, src, dst) <- cmdlineOpts argv
  [rc]  <- runX (application al src dst)
  if rc >= c_err
    then exitWith (ExitFailure (0-1))
    else exitWith ExitSuccess
 
-- XXX use CmdArgs
cmdlineOpts 	:: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = return ([withValidate no, withHTTP []], argv!!0, argv!!1)
 
application	:: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst
    = configSysVars cfg                                           -- (0)
      >>>
      readDocument [] src
      >>>
      processChildren (processDocumentRootElement `when` isElem)  -- (1)
      >>>
      writeDocument [] dst                                        -- (3)
      >>>
      getErrStatus
  
processDocumentRootElement	:: IOSArrow XmlTree XmlTree
processDocumentRootElement
    = xmlToHtml
      
