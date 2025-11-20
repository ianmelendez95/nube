module Nube.JSCtx
  ( ctxAssignArgStmt,
    ctxCallStmt,
    ctxFrameVar,
    ctxArg,
    ctxDotMember,
    ctx_var_name,
    ctx_var_text,
  )
where

import Data.Text qualified as T
import Nube.Syntax qualified as S

ctxAssignArgStmt :: T.Text -> Int -> S.Stmt
ctxAssignArgStmt var_name arg_idx =
  S.SAssign (ctxFrameVar var_name) (ctxArg arg_idx)

ctxCallStmt :: T.Text -> [S.Expr] -> T.Text -> S.Stmt
ctxCallStmt fn_name args cont_name =
  S.SExpr
    ( S.ECall
        (S.dotMemberExpr ctx_var_name "callCC")
        [S.EStringLit fn_name, S.EListLit args, S.EStringLit cont_name]
    )

-- | ctxFrameVar "foo" = _ctx.frame.foo
ctxFrameVar :: T.Text -> S.Expr
ctxFrameVar v = S.dotMembers ctx_var_name ["frame", v]

ctxArg :: Int -> S.Expr
ctxArg arg_idx = S.mkBracketMember (ctxDotMember "args") (S.ENumberLit arg_idx)

ctxDotMember :: T.Text -> S.Expr
ctxDotMember = S.dotMemberExpr ctx_var_name

ctx_var_name :: S.Expr
ctx_var_name = S.EVar ctx_var_text

-- | "_ctx"
ctx_var_text :: T.Text
ctx_var_text = "_ctx"