module Compile.FrameRefs (tStatement) where 

import Compile.Compiler (CContext (..), Compiler)
-- import Debug.Trace (trace, traceShowId)

import Compile.Cont (splitStmtContinuations)
import Compile.JSCtx (ctxDotMember, ctxFrameVar)
import Control.Monad.Except (MonadError (throwError), runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text qualified as T
import JS.Syntax qualified as S

tStatement :: S.Stmt -> Compiler S.Stmt
tStatement (S.SReturn e) = tReturn e
tStatement (S.SConst var rhs) = tAssign var rhs
tStatement (S.SAssign _ _) = throwError "Reassignment is not allowed, use a new const var"
tStatement (S.SExpr _) = throwError "Expression statements are not allowed"

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