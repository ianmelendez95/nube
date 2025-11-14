module JS.Transpile where

import Data.Text qualified as T
import JS.Syntax qualified as S

transpileStatement :: S.Stmt -> Either String S.Stmt
transpileStatement (S.SReturn e) = Right $ ctxReturn e
transpileStatement (S.SConst var rhs) = _
transpileStatement (S.SExpr _) = Left "Expression statements are not allowed"

ctxAssign :: T.Text -> S.Expr -> S.Stmt
ctxAssign var = S.SAssign (S.EMember ctx_var (S.EDotAccess var))

ctxReturn :: S.Expr -> S.Stmt
ctxReturn e = S.SExpr (S.ECall (S.EMember ctx_var (S.EDotAccess "return")) [e])

ctx_var :: S.Expr
ctx_var = S.EVar "_ctx"