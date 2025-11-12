{-# LANGUAGE OverloadedStrings #-}


module JS.Syntax (
  Script(..),
  Fn(..),
  FunBody(..),
  scriptText,
  funText
) where 

import qualified Data.Text as T

data Script = Script {
  scriptName :: T.Text,  -- file basename
  scriptFuns :: [Fn]
}

data Fn = Fn { 
  funName   :: T.Text,
  funParams :: T.Text,
  funBody   :: T.Text
}

data FunBody = FunBody {
  funStmts :: T.Text
}

instance Show Script where 
  show = T.unpack . scriptText

instance Show Fn where 
  show = T.unpack . funText

scriptText :: Script -> T.Text
scriptText (Script name funcs) = T.unlines $ name : map funText funcs

funText :: Fn -> T.Text
funText (Fn name params body) = "async function " <> name <> params <> " " <> body
