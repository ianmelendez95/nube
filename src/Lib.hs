{-# LANGUAGE OverloadedStrings #-}


module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Gen.Lambda as GL
import qualified JS.Parse as P
import qualified JS.Syntax as S
import qualified Gen.CF as CF

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BS

import System.FilePath
import qualified Gen.CF as CF

js_file = "example/capitalizeWords/capitalizeWords.js"

yaml_file = "example/capitalizeWords/dist-bck/aws-stack.yaml"

json_file = "example/capitalizeWords/dist-bck/aws-stack.json"

main :: IO ()
main = compileFile
-- main = convertYaml

compileFile :: IO ()
compileFile = do
  script <- P.parseJsFile js_file
  let js = S.scriptFuncs script
      scripts = GL.jsFunsToScripts js
      deploy_script = GL.jsFunsToDeployScript js
  GL.writeScripts (takeDirectory js_file </> "dist") deploy_script scripts

  let cf_txt = CF.templateFromScript script
  TIO.writeFile (templateFilePath $ S.scriptName script) cf_txt
  where 
    templateFilePath script_name = 
      dist_dir </> T.unpack (script_name <> "-template.json")

    dist_dir = takeDirectory js_file </> "dist"


-- convertYaml :: IO ()
-- convertYaml = do 
--   yaml <- readYamlFile yaml_file
--   let json_txt = J.encode yaml
--   BS.writeFile json_file json_txt
  -- where 
  --   readYamlFile :: FilePath -> IO J.Value
  --   readYamlFile = Y.decodeFileThrow


