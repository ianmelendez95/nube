module JS.Transpile
  ( Transpiler,
    TContext (..),
    ContSplit (..),
    transpileStatement,
    runTranspiler,
  )
where

import Compile.Compiler (TContext (..), Transpiler)
-- import Debug.Trace (trace, traceShowId)

import Compile.Cont (splitStmtContinuations)
import Control.Monad.Except (MonadError (throwError), runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text qualified as T
import JS.Syntax qualified as S
import Compile.JSCtx ( ctxFrameVar, ctxDotMember ) 

data ContSplit
  = ContBlock [S.Stmt]
  | ContCall
      { _contCallVar :: T.Text,
        _contCallFn :: T.Text,
        _contCallArgs :: [S.Expr]
      }
  deriving (Show)

transpileScript :: S.Script -> Either String S.Script
transpileScript (S.Script name fns) =
  let ctx = TContext (map S.fnName fns)
   in runTranspiler ctx $ do
        fns' <- mconcat <$> mapM transpileFn fns
        pure $ S.Script name fns'

transpileFn = undefined

tContStatements :: [S.Stmt] -> Transpiler [[S.Stmt]]
tContStatements orig_stmts = do
  split_stmts <- splitStmtContinuations orig_stmts
  undefined

-- where
--   tContInSplits :: [[S.Stmt]] -> [[S.Stmt]]
--   tContInSplits (head_stmts : [call_stmt] : rest_stmts) =
--     let call_fn :: S.Stmt
--         call_fn = _
--      in _
--   tContInSplits _ = _

transpileStatement :: TContext -> S.Stmt -> Either String S.Stmt
transpileStatement ctx = runTranspiler ctx . tStatement

runTranspiler :: TContext -> Transpiler a -> Either String a
runTranspiler context transpiler = runExcept $ runReaderT transpiler context

tStatement :: S.Stmt -> Transpiler S.Stmt
tStatement (S.SReturn e) = tReturn e
tStatement (S.SConst var rhs) = tAssign var rhs
tStatement (S.SAssign _ _) = throwError "Reassignment is not allowed, use a new const var"
tStatement (S.SExpr _) = throwError "Expression statements are not allowed"

tExpr :: S.Expr -> Transpiler S.Expr
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

tAssign :: T.Text -> S.Expr -> Transpiler S.Stmt
tAssign var rhs = S.SAssign <$> tVar var <*> pure rhs

tVar :: T.Text -> Transpiler S.Expr
tVar v = pure $ ctxFrameVar v

tReturn :: S.Expr -> Transpiler S.Stmt
tReturn e = do
  e' <- tExpr e
  pure $ S.SExpr (S.ECall (ctxDotMember "return") [e'])
