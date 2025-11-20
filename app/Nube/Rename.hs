module Nube.Rename
  ( renameInScript,
    renameInStatement,
    rExpr,
  )
where

-- import Debug.Trace (trace, traceShowId)

import Data.Text qualified as T
import Nube.Compiler (Compiler)
import Nube.JSCtx (ctxDotMember, ctxFrameVar, ctx_var_text)
import Nube.Syntax qualified as S

renameInScript :: S.Script -> Compiler S.Script
renameInScript = S.mapScriptStmtsM renameInStatement

renameInStatement :: S.Stmt -> Compiler S.Stmt
renameInStatement (S.SReturn e) = rReturn e
renameInStatement (S.SConst var rhs) = rAssign var rhs
renameInStatement (S.SAssign lhs rhs) = S.SAssign <$> rExpr lhs <*> rExpr rhs
renameInStatement (S.SExpr e) = S.SExpr <$> rExpr e

rExpr :: S.Expr -> Compiler S.Expr
rExpr (S.EVar v) = rVar v
rExpr (S.ECall lhs args) =
  S.ECall <$> rExpr lhs <*> traverse rExpr args
rExpr (S.EMember lhs (S.MBracketAccess rhs)) =
  S.EMember <$> rExpr lhs <*> (S.MBracketAccess <$> rExpr rhs)
rExpr (S.EMember lhs dotAccess) =
  S.EMember <$> rExpr lhs <*> pure dotAccess
rExpr (S.EInfix op lhs rhs) =
  S.EInfix op <$> rExpr lhs <*> rExpr rhs
rExpr (S.EListLit arg_exprs) = S.EListLit <$> mapM rExpr arg_exprs
rExpr s@(S.EStringLit _) = pure s
rExpr n@(S.ENumberLit _) = pure n

rAssign :: T.Text -> S.Expr -> Compiler S.Stmt
rAssign var rhs = S.SAssign <$> rVar var <*> rExpr rhs

rVar :: T.Text -> Compiler S.Expr
rVar v =
  pure $
    if v == ctx_var_text
      then S.EVar v
      else ctxFrameVar v

rReturn :: S.Expr -> Compiler S.Stmt
rReturn e = do
  e' <- rExpr e
  pure $ S.SExpr (S.ECall (ctxDotMember "return") [e'])