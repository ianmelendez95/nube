{-# LANGUAGE TemplateHaskell #-}

module Gen.Lambda
  ( Script (..),
    writeScripts,
    jsFunsToScripts,
    jsScriptToDeployScript,
    templateNameFromScriptName,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TLazy
import Debug.Trace (traceId)
import Gen.CF qualified as CF
import Nube.Syntax (Fn (fnName))
import Nube.Syntax qualified as S
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import Text.Julius hiding (renderJavascript)

data Script = Script
  { scriptName :: T.Text,
    scriptContent :: T.Text
  }

writeScripts :: FilePath -> T.Text -> [Script] -> IO ()
writeScripts dist deploy_script handler_scripts = do
  writeDistFile "deploy.sh" deploy_script
  copyToDist "template/js/runtime" "nodejs/node_modules/runtime"
  mapM_ doScript handler_scripts
  where
    doScript :: Script -> IO ()
    doScript script =
      writeDistFile
        (T.unpack (scriptName script) <> "/index.mjs")
        (scriptContent script)

    writeDistFile :: FilePath -> T.Text -> IO ()
    writeDistFile file_path content =
      let dest = dist </> file_path
       in do
            createDirectoryIfMissing True (takeDirectory dest)
            TIO.writeFile dest content

    copyToDist :: FilePath -> FilePath -> IO ()
    copyToDist src_dir dist_dir = copyPath src_dir (dist </> dist_dir)

copyPath :: FilePath -> FilePath -> IO ()
copyPath src_path dest_path = do
  is_dir <- doesDirectoryExist src_path
  if is_dir
    then do
      createDirectoryIfMissing True dest_path
      src_subpaths <- listDirectory src_path
      mapM_ (\src_entry -> copyPath (src_path </> src_entry) (dest_path </> src_entry)) src_subpaths
    else do
      copyFile (traceId src_path) dest_path

jsScriptToDeployScript :: S.Script -> T.Text
jsScriptToDeployScript script =
  let name = S.scriptName script
      funs = S.scriptFns script
   in mkDeployScript
        (CF.bucketNameFromScriptName name)
        (templateNameFromScriptName name)
        (name <> "-stack")
        (name <> "-layer")
        (map S.fnName funs)

templateNameFromScriptName :: T.Text -> T.Text
templateNameFromScriptName name = name <> "-template.json"

jsFunsToScripts :: [S.Fn] -> [Script]
jsFunsToScripts = map jsFunsToScript

jsFunsToScript :: S.Fn -> Script
jsFunsToScript main_fun@(S.Fn fn_name _ _) =
  let content =
        T.intercalate
          "\n\n"
          [ mkHandlerDecl fn_name,
            S.fnText main_fun
          ]
   in Script fn_name content

-- Proxies

mkHandlerDecl :: T.Text -> T.Text
mkHandlerDecl impl_fun_name = renderJavascript $(juliusFile "template/js/handler.julius")

mkDeployScript ::
  T.Text ->
  T.Text ->
  T.Text ->
  T.Text ->
  [T.Text] ->
  T.Text
mkDeployScript bucket_name template_name stack_name layer_name fun_names_arr =
  let fun_names = T.unlines fun_names_arr
   in renderJavascript $(juliusFile "template/bash/deploy.julius")

renderJavascript :: JavascriptUrl a -> T.Text
renderJavascript = TLazy.toStrict . renderJavascriptUrl (\_ _ -> error "Can't resolve URLs")
