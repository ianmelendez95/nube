{-# LANGUAGE OverloadedStrings #-}

module JS.Syntax
  ( Script (..),
    Fn (..),
    Stmt (..),
    Expr (..),
    EAccess (..),
    scriptText,
    funText,
    IOp (..),
  )
where

import Data.Text qualified as T

data Script = Script
  { scriptName :: T.Text, -- file basename
    scriptFuns :: [Fn]
  }

data Fn = Fn
  { fnName :: T.Text,
    fnParams :: [T.Text],
    fnStmts :: [Stmt]
  }
  deriving (Eq)

data Stmt
  = SAssign T.Text Expr
  | SReturn Expr
  | SExpr Expr
  deriving (Eq)

data Expr
  = EVar T.Text
  | EStringLit T.Text
  | ENumberLit Int
  | ECall Expr [Expr]
  | EMember Expr EAccess
  | EInfix IOp Expr Expr
  deriving (Eq)

data IOp = IPlus
  deriving (Show, Eq)

data EAccess
  = EDotAccess T.Text
  | EBracketAccess Expr
  deriving (Show, Eq)

instance Show Script where
  show = T.unpack . scriptText

instance Show Fn where
  show = T.unpack . funText

instance Show Stmt where
  show = T.unpack . stmtText

instance Show Expr where
  show = T.unpack . exprText

scriptText :: Script -> T.Text
scriptText (Script name funcs) = T.unlines $ name : map funText funcs

funText :: Fn -> T.Text
funText (Fn name params body) = "function " <> name <> params_text <> " {\n" <> body_text <> "\n}"
  where
    params_text = "(" <> T.intercalate ", " params <> ")"
    body_text = T.intercalate ";\n  " (map stmtText body)

stmtText :: Stmt -> T.Text
stmtText (SAssign var rhs) = "const " <> var <> " = " <> exprText rhs <> ";"
stmtText (SReturn rhs) = "return " <> exprText rhs <> ";"

exprText :: Expr -> T.Text
exprText (EVar v) = v
exprText (EStringLit s) = "\"" <> s <> "\""
exprText (ENumberLit n) = T.show n
exprText (ECall lhs args) = exprText lhs <> "(" <> T.intercalate ", " (map exprText args) <> ")"
exprText (EMember lhs rhs) = exprText lhs <> accessText rhs
exprText (EInfix op lhs rhs) = exprText lhs <> opText op <> exprText rhs

accessText :: EAccess -> T.Text
accessText (EDotAccess v) = "." <> v
accessText (EBracketAccess rhs) = "[" <> exprText rhs <> "]"

opText :: IOp -> T.Text
opText IPlus = "+"
