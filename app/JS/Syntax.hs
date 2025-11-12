{-# LANGUAGE OverloadedStrings #-}


module JS.Syntax (
  Script(..),
  Fn(..),
  Stmt(..),
  Expr(..),
  MemberExpr(..),
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
  fnParams :: T.Text,
  fnStmts  :: [Stmt]
}

data Stmt 
  = SAssign T.Text Expr
  | SReturn Expr

data Expr 
  = EVar T.Text
  | EStringLit T.Text
  | ENumberLit Double
  | ECall Expr [Expr] 
  | EMember MemberExpr
  | EAwait Expr
  deriving (Show, Eq)

data MemberExpr 
  = DotMember T.Text T.Text
  | BracketMember T.Text Expr
  deriving (Show, Eq)

instance Show Script where 
  show = T.unpack . scriptText

instance Show Fn where 
  show = T.unpack . funText

scriptText :: Script -> T.Text
scriptText (Script name funcs) = T.unlines $ name : map funText funcs

funText :: Fn -> T.Text
funText (Fn name params body) = "async function " <> name <> params <> " " <> stmtText body

stmtText :: [Stmt] -> T.Text
stmtText = undefined
