{-# LANGUAGE OverloadedStrings #-}

module Lambda.Gen where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified JS.Syntax as S

import System.Directory
import JS.Syntax (Fun(funName))

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
        , "const { Buffer } = require('node:buffer')"
        , ""
        , "exports.handler = " <> jsFunToHandler main_fun 
        , S.funText main_fun
        , ""
        ] ++ map jsFunToProxy helper_funs
   in Script (funName main_fun) content

jsFunToHandler :: S.Fun -> T.Text
jsFunToHandler fun = mkHandlerFun (S.funName fun)

jsFunToProxy :: S.Fun -> T.Text
jsFunToProxy fun = mkProxyFun (S.funName fun) (S.funParams fun)

mkHandlerFun :: T.Text -> T.Text 
mkHandlerFun impl_fun_name = T.unlines
  [ "async function(event) {"
  , "  try {"
  , "    return {"
  , "      statusCode: 200,"
  , "      body: JSON.stringify(await " <> impl_fun_name <> ".apply(null, JSON.parse(event.body)))"
  , "    }"
  , "  } catch (e) {"
  , "    console.log(e)"
  , "    return e"
  , "  }"
  , "}"
  ]

mkProxyFun :: T.Text -> T.Text -> T.Text
mkProxyFun impl_fun_name impl_fun_params = T.unlines 
  [ "async function " <> impl_fun_name <> impl_fun_params <> " {"
  , "  const argsString = JSON.stringify("
  , "    Array.from(arguments).slice(0, " <> impl_fun_name <> ".length))"
  , ""
  , "  const options = {"
  , "    hostname: process.env.AWS_GATEWAY_HOST,"
  , "    port: 443,"
  , "    path: '/" <> impl_fun_name <> "',"
  , "    method: 'POST'"
  , "  };"
  , ""
  , "  return new Promise((resolve, reject) => {"
  , "    const req = https.request(options, (res) => {"
  , "      let data = ''"
  , ""
  , "      res.on('data', (chunk) => {"
  , "        data += chunk"
  , "      })"
  , ""
  , "      res.on('end', () => {"
  , "        resolve(JSON.parse(data))"
  , "      })"
  , ""
  , "      res.on('error', (e) => {"
  , "        console.error('Response Error: ', e)"
  , "        reject(e)"
  , "      })"
  , "    })"
  , ""
  , "    req.setHeader('Content-Type', 'application/json')"
  , "    req.setHeader('Content-Length', Buffer.byteLength(argsString))"
  , ""
  , "    req.on('error', (e) => {"
  , "      console.error('Request Error: ', e)"
  , "      reject(e)"
  , "    })"
  , ""
  , "    req.end(argsString)"
  , "  })"
  , "}"
  ]

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