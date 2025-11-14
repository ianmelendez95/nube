module JS.Transpile
  ( ctx_var_name,
    transpileStatement,
  )
where

import Data.Text qualified as T
import JS.Syntax qualified as S

transpileStatement :: S.Stmt -> Either String S.Stmt
transpileStatement (S.SReturn e) = ctxReturn <$> transpileExpr e
transpileStatement (S.SConst var rhs) = transpileAssign var rhs
transpileStatement (S.SAssign _ _) = Left "Reassignment is not allowed, use a new const var"
transpileStatement (S.SExpr _) = Left "Expression statements are not allowed"

transpileExpr :: S.Expr -> Either String S.Expr
transpileExpr (S.EVar v) = transpileVar v
transpileExpr (S.ECall lhs args) =
  S.ECall <$> transpileExpr lhs <*> traverse transpileExpr args
transpileExpr (S.EMember lhs (S.MBracketAccess rhs)) =
  S.EMember <$> transpileExpr lhs <*> (S.MBracketAccess <$> transpileExpr rhs)
transpileExpr (S.EInfix op lhs rhs) =
  S.EInfix op <$> transpileExpr lhs <*> transpileExpr rhs
transpileExpr e = Right e

transpileAssign :: T.Text -> S.Expr -> Either String S.Stmt
transpileAssign var rhs = S.SAssign <$> transpileVar var <*> Right rhs

transpileVar :: T.Text -> Either String S.Expr
transpileVar v = Right $ S.EMember (ctxDotMember v) (S.MDotAccess v)

ctxReturn :: S.Expr -> S.Stmt
ctxReturn e = S.SExpr (S.ECall (ctxDotMember "return") [e])

ctxDotMember :: T.Text -> S.Expr
ctxDotMember prop = S.EMember ctx_var_name (S.MDotAccess prop)

ctx_var_name :: S.Expr
ctx_var_name = S.EVar "_ctx"