{-# LANGUAGE OverloadedStrings #-}

module Lambda.Gen where 

import qualified Data.Text as T
import qualified JS.Syntax as S

jsFunsToScript :: S.Fun -> [S.Fun] -> T.Text
jsFunsToScript main_fun helper_funs = 
  T.unlines $ [ "const https = require('https')"
              , "const { Buffer } = require('node:buffer')"
              , ""
              , "exports.handler = " <> jsFunToHandler main_fun 
              , S.funText main_fun
              ] ++ map jsFunToProxy helper_funs

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