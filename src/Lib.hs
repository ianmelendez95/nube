module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Gen.Lambda as GL
import qualified JS.Parse as P
import qualified JS.Syntax as S

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BS

import System.FilePath

js_file = "example/capitalizeWords/src.js"

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

-- convertYaml :: IO ()
-- convertYaml = do 
--   yaml <- readYamlFile yaml_file
--   let json_txt = J.encode yaml
--   BS.writeFile json_file json_txt
  -- where 
  --   readYamlFile :: FilePath -> IO J.Value
  --   readYamlFile = Y.decodeFileThrow


