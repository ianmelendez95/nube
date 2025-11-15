module Compile where

import Control.Monad
  ( unless,
  )
import Data.Char
import Data.Text qualified as T
import Gen.CF qualified as CF
import Gen.Lambda qualified as GL
import JS.Parse qualified as P
import JS.Syntax qualified as S
import System.FilePath
import Util.Aeson

-- test_js_file = "example/capitalizeWords/capitalizeWords.js"

compileFile :: FilePath -> IO ()
compileFile js_file = do
  assertValidJsFileName js_file

  script <- P.parseJsFile js_file
  let js = S.scriptFns script
      proxies_script = GL.jsFunsToProxiesScript js
      scripts = GL.jsFunsToScripts js
      deploy_script = GL.jsScriptToDeployScript script
  GL.writeScripts
    (takeDirectory js_file </> "dist")
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
          unless
            (all isLetter basename)
            (error $ "Only letters allowed in file basename: " <> basename)
