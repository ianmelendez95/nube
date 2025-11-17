module Compile.JSCtx
  ( ctxAssignArgStmt,
    ctxCallStmt,
    ctxFrameVar,
    ctxArg,
    ctxDotMember,
    ctx_var_name,
  )
where

import Compile.Syntax qualified as S
import Data.Text qualified as T

ctxAssignArgStmt :: T.Text -> Int -> S.Stmt
ctxAssignArgStmt var_name arg_idx =
  S.SAssign (ctxFrameVar var_name) (ctxArg arg_idx)

ctxCallStmt :: T.Text -> [S.Expr] -> T.Text -> S.Stmt
ctxCallStmt fn_name args cont_name =
  S.SExpr (S.ECall (S.dotMemberExpr ctx_var_name fn_name) [S.EVar fn_name, S.EListLit args, S.EVar cont_name])

ctxFrameVar :: T.Text -> S.Expr
ctxFrameVar v = S.dotMembers ctx_var_name ["frame", v]

ctxArg :: Int -> S.Expr
ctxArg arg_idx = S.mkBracketMember (ctxDotMember "args") (S.ENumberLit arg_idx)

ctxDotMember :: T.Text -> S.Expr
ctxDotMember = S.dotMemberExpr ctx_var_name

ctx_var_name :: S.Expr
ctx_var_name = S.EVar "_ctx"