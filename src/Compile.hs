{-# LANGUAGE OverloadedStrings #-}

module Compile where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char

import qualified Gen.Lambda as GL
import qualified JS.Parse as P
import qualified JS.Syntax as S
import qualified Gen.CF as CF

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BS

import System.FilePath
import qualified Gen.CF as CF

import Util.Aeson

-- test_js_file = "example/capitalizeWords/capitalizeWords.js"

compileFile :: FilePath -> IO ()
compileFile js_file = do
  assertValidJsFileName js_file

  script <- P.parseJsFile js_file
  let js = S.scriptFuns script
      scripts = GL.jsFunsToScripts js
      deploy_script = GL.jsScriptToDeployScript script
  GL.writeScripts (takeDirectory js_file </> "dist") deploy_script scripts

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
        else if not (all isLetter basename)
               then error $ "Only letters allowed in file basename: " <> basename
               else pure ()
