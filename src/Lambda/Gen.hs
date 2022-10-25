{-# LANGUAGE OverloadedStrings #-}

module Lambda.Gen where 

import qualified Data.Text as T
import qualified JS.Syntax as S

jsFunToHandlerScript :: S.Fun -> T.Text
jsFunToHandlerScript fun = mkHandlerFun (S.funName fun) <> "\n" <> T.pack (show fun)

mkHandlerFun :: T.Text -> T.Text 
mkHandlerFun impl_fun_name = T.unlines
  [ "exports.handler = async function(event) {"
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