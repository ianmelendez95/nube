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
import Gen.CF qualified as CF
import Nube.Syntax (Fn (fnName))
import Nube.Syntax qualified as S
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Text.Julius hiding (renderJavascript)

data Script = Script
  { scriptName :: T.Text,
    scriptContent :: T.Text
  }

writeScripts :: FilePath -> T.Text -> [Script] -> IO ()
writeScripts dist deploy_script handler_scripts = do
  writeDistFile "deploy.sh" deploy_script
  copyPath "template/js/runtime" "nodejs/node_modules/runtime"
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

copyPath :: FilePath -> FilePath -> IO ()
copyPath src_path dest_path = do
  is_dir <- doesDirectoryExist src_path
  if is_dir
    then do
      createDirectoryIfMissing True dest_path
      src_subpaths <- listDirectory src_path
      mapM_ (\sub_path -> copyPath sub_path (dest_path </> takeFileName sub_path)) src_subpaths
    else do
      dest_exists <- doesFileExist dest_path
      if dest_exists
        then pure ()
        else copyFile src_path dest_path

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
jsFunsToScripts = go []
  where
    go _ [] = []
    go xs (y : ys) = jsFunsToScript y (xs ++ ys) : go (y : xs) ys

jsFunsToScript :: S.Fn -> [S.Fn] -> Script
jsFunsToScript main_fun helper_funs =
  let content =
        T.intercalate
          "\n\n"
          [ jsFunsToProxiesImport helper_funs,
            jsFunToHandler main_fun,
            S.funText main_fun
          ]
   in Script (fnName main_fun) content

-- Proxies

jsFunsToProxiesImport :: [S.Fn] -> T.Text
jsFunsToProxiesImport funs =
  "import {\n  "
    <> T.intercalate ",\n  " ("sqsClient" : "dynamoClient" : map S.fnName funs)
    <> "\n} from 'proxies'"
    <> "\nimport {PutItemCommand} from '@aws-sdk/client-dynamodb';"

jsFunToHandler :: S.Fn -> T.Text
jsFunToHandler fn = mkHandlerFun (S.fnName fn)

jsFunToProxy :: S.Fn -> T.Text
jsFunToProxy fn = mkProxyFun (S.fnName fn) undefined

mkHandlerFun :: T.Text -> T.Text
mkHandlerFun impl_fun_name = renderJavascript $(juliusFile "template/js/handler.julius")

mkProxyFun :: T.Text -> T.Text -> T.Text
mkProxyFun impl_fun_name impl_fun_params = renderJavascript $(juliusFile "template/js/proxy.julius")

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
