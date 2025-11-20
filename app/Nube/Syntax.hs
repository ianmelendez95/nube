module Nube.Syntax
  ( Script (..),
    Fn (..),
    Stmt (..),
    Expr (..),
    MAccess (..),
    mapScriptStmtsM,
    mapScriptFnsM,
    scriptText,
    funText,
    scriptFns',
    fnStmts',
    IOp (..),
    dotMemberExpr,
    dotMembers,
    mkBracketMember,
  )
where

import Control.Lens (Lens', lens)
import Data.Text qualified as T

data Script = Script
  { scriptName :: T.Text, -- file basename
    scriptFns :: [Fn]
  }

data Fn = Fn
  { fnName :: T.Text,
    fnParams :: [T.Text],
    fnStmts :: [Stmt]
  }
  deriving (Eq)

data Stmt
  = SConst T.Text Expr
  | SAssign Expr Expr
  | SReturn Expr
  | SExpr Expr
  deriving (Eq)

data Expr
  = EVar T.Text
  | EListLit [Expr]
  | EStringLit T.Text
  | ENumberLit Int
  | ECall Expr [Expr]
  | EMember Expr MAccess
  | EInfix IOp Expr Expr
  deriving (Eq)

data IOp = IPlus
  deriving (Show, Eq)

data MAccess
  = MDotAccess T.Text
  | MBracketAccess Expr
  deriving (Show, Eq)

instance Show Script where
  show = T.unpack . scriptText

instance Show Fn where
  show = T.unpack . funText

instance Show Stmt where
  show = T.unpack . stmtText

instance Show Expr where
  show = T.unpack . exprText

scriptFns' :: Lens' Script [Fn]
scriptFns' = lens scriptFns (\script fns -> script {scriptFns = fns})

fnStmts' :: Lens' Fn [Stmt]
fnStmts' = lens fnStmts (\fn stmts -> fn {fnStmts = stmts})

mapScriptStmtsM :: (Monad m) => (Stmt -> m Stmt) -> Script -> m Script
mapScriptStmtsM = mapScriptFnsM . mapFnStmtsM

mapScriptFnsM :: (Monad m) => (Fn -> m Fn) -> Script -> m Script
mapScriptFnsM fn_m (Script s_name s_fns) =
  Script s_name <$> mapM fn_m s_fns

mapFnStmtsM :: (Monad m) => (Stmt -> m Stmt) -> Fn -> m Fn
mapFnStmtsM stmt_fn_m (Fn f_name f_params f_stmts) =
  Fn f_name f_params <$> mapM stmt_fn_m f_stmts

renameInFn :: (Monad m) => (Stmt -> m Stmt) -> Fn -> m Fn
renameInFn fM (Fn f_name f_params f_stmts) = Fn f_name f_params <$> mapM fM f_stmts

scriptText :: Script -> T.Text
scriptText (Script name funcs) = T.unlines $ name : map funText funcs

funText :: Fn -> T.Text
funText (Fn name params body) = "function " <> name <> params_text <> " {\n  " <> body_text <> ";\n}"
  where
    params_text = "(" <> T.intercalate ", " params <> ")"
    body_text = T.intercalate ";\n  " (map stmtText body)

stmtText :: Stmt -> T.Text
stmtText (SConst var rhs) = "const " <> var <> " = " <> exprText rhs
stmtText (SReturn rhs) = "return " <> exprText rhs
stmtText (SExpr e) = exprText e
stmtText (SAssign lhs rhs) = exprText lhs <> " = " <> exprText rhs

exprText :: Expr -> T.Text
exprText (EVar v) = v
exprText (EStringLit s) = "'" <> s <> "'"
exprText (ENumberLit n) = T.show n
exprText (ECall lhs args) = exprText lhs <> csExprs "(" ")" args
exprText (EMember lhs rhs) = exprText lhs <> accessText rhs
exprText (EInfix op lhs rhs) = exprText lhs <> " " <> opText op <> " " <> exprText rhs
exprText (EListLit xs) = csExprs "[" "]" xs

csExprs :: T.Text -> T.Text -> [Expr] -> T.Text
csExprs bra cket xs = bra <> T.intercalate ", " (map exprText xs) <> cket

accessText :: MAccess -> T.Text
accessText (MDotAccess v) = "." <> v
accessText (MBracketAccess rhs) = "[" <> exprText rhs <> "]"

dotMembers :: Expr -> [T.Text] -> Expr
dotMembers = foldl dotMemberExpr

dotMemberExpr :: Expr -> T.Text -> Expr
dotMemberExpr lhs var = EMember lhs (MDotAccess var)

mkBracketMember :: Expr -> Expr -> Expr
mkBracketMember lhs member_epxr = EMember lhs (MBracketAccess member_epxr)

opText :: IOp -> T.Text
opText IPlus = "+"
