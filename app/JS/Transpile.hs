module JS.Transpile
  ( Compiler,
    TContext (..),
    ContSplit (..),
    transpileStatement,
    runCompiler,
  )
where

import Compile.Compiler (Compiler, TContext (..))
-- import Debug.Trace (trace, traceShowId)

import Compile.Cont (splitStmtContinuations)
import Compile.JSCtx (ctxDotMember, ctxFrameVar)
import Control.Monad.Except (MonadError (throwError), runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text qualified as T
import JS.Syntax qualified as S

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
   in runCompiler ctx $ do
        fns' <- mconcat <$> mapM transpileFn fns
        pure $ S.Script name fns'

transpileFn = undefined

tContStatements :: [S.Stmt] -> Compiler [[S.Stmt]]
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
transpileStatement ctx = runCompiler ctx . tStatement

runCompiler :: TContext -> Compiler a -> Either String a
runCompiler context transpiler = runExcept $ runReaderT transpiler context

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
