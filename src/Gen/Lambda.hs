{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen.Lambda where 

import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.IO as TIO
import qualified JS.Syntax as S
import qualified Gen.CF as CF

import System.Directory
import JS.Syntax (Fun(funName))

import Text.Julius hiding (renderJavascript)

data Script = Script 
  { scriptName :: T.Text
  , scriptContent :: T.Text
  }

writeScripts :: FilePath -> T.Text -> [Script] -> IO ()
writeScripts dist deploy_script js_scripts = do
  createDirectoryIfMissing True dist
  writeDistFile "deploy.sh" deploy_script
  mapM_ doScript js_scripts
  where 
    doScript :: Script -> IO ()
    doScript script = writeDistFile (T.unpack (scriptName script) <> ".js") 
                                    (scriptContent script)
    
    writeDistFile :: FilePath -> T.Text -> IO ()
    writeDistFile file_name =
      TIO.writeFile (dist <> "/" <> file_name)

jsScriptToDeployScript :: S.Script -> T.Text
jsScriptToDeployScript script = 
  let name = S.scriptName script
      funs = S.scriptFuns script 
   in mkDeployScript (CF.bucketNameFromScriptName name)
                     (templateNameFromScriptName name)
                     (name <> "-stack")
                     (map S.funName funs)

templateNameFromScriptName :: T.Text -> T.Text
templateNameFromScriptName name = name <> "-template.json"

jsFunsToScripts :: [S.Fun] -> [Script]
jsFunsToScripts = go []
  where 
    go _ [] = []
    go xs (y:ys) = jsFunsToScript y (xs ++ ys) : go (y:xs) ys

jsFunsToScript :: S.Fun -> [S.Fun] -> Script
jsFunsToScript main_fun helper_funs = 
  let content = T.unlines $
        [ "const https = require('https')"
        , "const { Buffer } = require('node:buffer')\n"
        , "exports.handler = " <> jsFunToHandler main_fun 
        , ""
        , S.funText main_fun
        , ""
        ] ++ map jsFunToProxy helper_funs
   in Script (funName main_fun) content

jsFunToHandler :: S.Fun -> T.Text
jsFunToHandler fun = mkHandlerFun (S.funName fun)

jsFunToProxy :: S.Fun -> T.Text
jsFunToProxy fun = mkProxyFun (S.funName fun) (S.funParams fun)

mkHandlerFun :: T.Text -> T.Text 
mkHandlerFun impl_fun_name = renderJavascript $(juliusFile "template/js/handler.julius")

mkProxyFun :: T.Text -> T.Text -> T.Text
mkProxyFun impl_fun_name impl_fun_params = renderJavascript $(juliusFile "template/js/proxy.julius")

mkDeployScript :: T.Text 
               -> T.Text 
               -> T.Text
               -> [T.Text] 
               -> T.Text
mkDeployScript bucket_name template_name stack_name fun_names_arr = 
  let fun_names = T.unlines fun_names_arr
   in renderJavascript $(juliusFile "template/bash/deploy.julius")

renderJavascript :: JavascriptUrl a -> T.Text
renderJavascript = TLazy.toStrict . renderJavascriptUrl (\_ _ -> error "Can't resolve URLs") 
