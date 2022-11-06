{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen.Lambda where 

import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.IO as TIO
import qualified JS.Syntax as S

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

jsFunsToDeployScript :: [S.Fun] -> T.Text
jsFunsToDeployScript funs = mkDeployScript (map funName funs)

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

mkDeployScript :: [T.Text] -> T.Text
mkDeployScript names = T.unlines $ 
  [ "#!/usr/bin/env bash "
  , ""
  , "ROLE_ARN=\"arn:aws:iam::565652071321:role/service-role/hello-aws-role-aoys6qk7\""
  , ""
  , "package () {"
  , "  FUN_NAME=\"$1\""
  , ""
  , "  ZIP_FILE=\"$FUN_NAME-aws.zip\""
  , ""
  , "  echo \"Creating ZIP package: $ZIP_FILE\""
  , "  zip \"$ZIP_FILE\" \"$FUN_NAME.js\""
  , "}"
  , ""
  , "create () {"
  , "  FUN_NAME=\"$1\""
  , ""
  , "  echo \"Creating: $FUN_NAME\""
  , ""
  , "  ZIP_FILE=\"$FUN_NAME-aws.zip\""
  , ""
  , "  aws lambda create-function \\"
  , "    --function-name \"$FUN_NAME\" \\"
  , "    --role \"$ROLE_ARN\" \\"
  , "    --runtime nodejs16.x \\"
  , "    --package-type Zip \\"
  , "    --zip-file \"fileb://$ZIP_FILE\" \\"
  , "    --handler \"$FUN_NAME.handler\" \\"
  , "    > \"$FUN_NAME-result.json\" 2>&1"
  , "}"
  , ""
  , "update () {"
  , "  FUN_NAME=\"$1\""
  , ""
  , "  echo \"Updating: $FUN_NAME\""
  , ""
  , "  ZIP_FILE=\"$FUN_NAME-aws.zip\""
  , ""
  , "  aws lambda update-function-code \\"
  , "    --function-name \"$FUN_NAME\" \\"
  , "    --zip-file \"fileb://$ZIP_FILE\" \\"
  , "    > \"$FUN_NAME-result.json\" 2>&1"
  , "}"
  , ""
  , "deploy () {"
  , "  FUN_NAME=\"$1\""
  , ""
  , "  echo \"Deploying: $FUN_NAME\""
  , ""
  , "  package $FUN_NAME"
  , ""
  , "  if aws lambda get-function --function-name \"$FUN_NAME\" > /dev/null; then "
  , "    update $FUN_NAME"
  , "  else "
  , "    create $FUN_NAME"
  , "  fi"
  , "}"
  , ""
  , "FUN_NAMES=\"" <> T.unlines names <> "\""
  , ""
  , "for f in $FUN_NAMES; do "
  , "  deploy \"$f\""
  , "done"
  ]

renderJavascript :: JavascriptUrl a -> T.Text
renderJavascript = TLazy.toStrict . renderJavascriptUrl (\_ _ -> error "Can't resolve URLs") 
