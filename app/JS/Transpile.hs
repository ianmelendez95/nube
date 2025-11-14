module JS.Transpile
  ( ctx_var,
    transpileStatement,
  )
where

import Data.Text qualified as T
import JS.Syntax qualified as S

transpileStatement :: S.Stmt -> Either String S.Stmt
transpileStatement (S.SReturn e) = Right $ ctxReturn e
transpileStatement (S.SConst var rhs) = Right $ ctxAssign var rhs
transpileStatement (S.SAssign _ _) = Left "Reassignment is not allowed, use a new const var"
transpileStatement (S.SExpr _) = Left "Expression statements are not allowed"

ctxAssign :: T.Text -> S.Expr -> S.Stmt
ctxAssign var = S.SAssign (S.EMember ctx_var (S.MDotAccess var))

ctxReturn :: S.Expr -> S.Stmt
ctxReturn e = S.SExpr (S.ECall (S.EMember ctx_var (S.MDotAccess "return")) [e])

ctx_var :: S.Expr
ctx_var = S.EVar "_ctx"