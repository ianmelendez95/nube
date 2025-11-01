{-# LANGUAGE OverloadedStrings #-}

module Compile where

import qualified Data.Text as T
import Data.Char

import qualified Gen.Lambda as GL
import qualified JS.Parse as P
import qualified JS.Syntax as S
import qualified Gen.CF as CF

import System.FilePath

import Control.Monad (
    unless
  )

import Util.Aeson

-- test_js_file = "example/capitalizeWords/capitalizeWords.js"

compileFile :: FilePath -> IO ()
compileFile js_file = do
  assertValidJsFileName js_file

  script <- P.parseJsFile js_file
  let js = S.scriptFuns script
      proxies_script = GL.jsFunsToProxiesScript js
      scripts = GL.jsFunsToScripts js
      deploy_script = GL.jsScriptToDeployScript script
  GL.writeScripts (takeDirectory js_file </> "dist") 
                  deploy_script 
                  proxies_script
                  scripts

  let template = CF.templateFromScript script
  writeFileJSON (templateFilePath $ S.scriptName script) template
  where 
    templateFilePath script_name = 
      dist_dir </> T.unpack (GL.templateNameFromScriptName script_name)

    dist_dir = takeDirectory js_file </> "dist"


assertValidJsFileName :: FilePath -> IO ()
assertValidJsFileName file_path = 
  let ext = takeExtension file_path
      basename = takeBaseName file_path
   in if ext /= ".js"
        then error $ "Expecting .js file extension - has: " <> ext
        else 
          unless (all isLetter basename)
                 (error $ "Only letters allowed in file basename: " <> basename)
