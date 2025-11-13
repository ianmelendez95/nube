{-# LANGUAGE OverloadedStrings #-}


module JS.Syntax (
  Script(..),
  Fn(..),
  Stmt(..),
  Expr(..),
  EAccess(..),
  scriptText,
  funText
) where 

import qualified Data.Text as T

data Script = Script {
  scriptName :: T.Text,  -- file basename
  scriptFuns :: [Fn]
}

data Fn = Fn { 
  fnName   :: T.Text,
  fnParams :: [T.Text],
  fnStmts  :: [Stmt]
}

data Stmt 
  = SAssign T.Text Expr
  | SReturn Expr
  deriving (Show, Eq)

data Expr 
  = EVar T.Text
  | EStringLit T.Text
  | ENumberLit Double
  | ECall Expr [Expr] 
  | EMember Expr EAccess
  | EAwait Expr
  deriving (Show, Eq)

data EAccess 
  = EDotAccess T.Text
  | EBracketAccess Expr
  deriving (Show, Eq)

instance Show Script where 
  show = T.unpack . scriptText

instance Show Fn where 
  show = T.unpack . funText

scriptText :: Script -> T.Text
scriptText (Script name funcs) = T.unlines $ name : map funText funcs

funText :: Fn -> T.Text
funText (Fn name params body) = "async function " <> name <> undefined <> " " <> stmtText body

stmtText :: [Stmt] -> T.Text
stmtText = undefined
