module Nube.Rename (renameInStatement) where

-- import Debug.Trace (trace, traceShowId)

import Control.Monad.Except (MonadError (throwError))
import Data.Text qualified as T
import Nube.Compiler (Compiler)
import Nube.JSCtx (ctxDotMember, ctxFrameVar)
import Nube.Syntax qualified as S

renameInStatement :: S.Stmt -> Compiler S.Stmt
renameInStatement (S.SReturn e) = tReturn e
renameInStatement (S.SConst var rhs) = tAssign var rhs
renameInStatement (S.SAssign _ _) = throwError "Reassignment is not allowed, use a new const var"
renameInStatement (S.SExpr _) = throwError "Expression statements are not allowed"

tExpr :: S.Expr -> Compiler S.Expr
tExpr (S.EVar v) = tVar v
tExpr (S.ECall lhs args) =
  S.ECall <$> tExpr lhs <*> traverse tExpr args
tExpr (S.EMember lhs (S.MBracketAccess rhs)) =
  S.EMember <$> tExpr lhs <*> (S.MBracketAccess <$> tExpr rhs)
tExpr (S.EMember lhs dotAccess) =
  S.EMember <$> tExpr lhs <*> pure dotAccess
tExpr (S.EInfix op lhs rhs) =
  S.EInfix op <$> tExpr lhs <*> tExpr rhs
tExpr e = pure e

tAssign :: T.Text -> S.Expr -> Compiler S.Stmt
tAssign var rhs = S.SAssign <$> tVar var <*> pure rhs

tVar :: T.Text -> Compiler S.Expr
tVar v = pure $ ctxFrameVar v

tReturn :: S.Expr -> Compiler S.Stmt
tReturn e = do
  e' <- tExpr e
  pure $ S.SExpr (S.ECall (ctxDotMember "return") [e'])