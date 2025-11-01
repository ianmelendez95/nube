{-# LANGUAGE OverloadedStrings #-}


module JS.Syntax where 

import qualified Data.Text as T

data Script = Script {
  scriptName :: T.Text,  -- file basename
  scriptFuns :: [Fun]
}

data Fun = Fun { 
  funName   :: T.Text,
  funParams :: T.Text,
  funBody   :: T.Text
}

instance Show Script where 
  show = T.unpack . scriptText

instance Show Fun where 
  show = T.unpack . funText

scriptText :: Script -> T.Text
scriptText (Script name funcs) = T.unlines $ name : map funText funcs

funText :: Fun -> T.Text
funText (Fun name params body) = "async function " <> name <> params <> " " <> body
