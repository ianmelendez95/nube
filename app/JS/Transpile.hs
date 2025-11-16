module JS.Transpile
  ( Compiler,
    CContext (..),
    ContSplit (..),
    transpileStatement,
    runCompiler,
  )
where

import Compile.Compiler (CContext (..), Compiler)
-- import Debug.Trace (trace, traceShowId)

import Compile.Cont (splitStmtContinuations)
import Compile.FrameRefs (tStatement)
import Control.Monad.Except (runExcept)
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
  let ctx = CContext (map S.fnName fns)
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

transpileStatement :: CContext -> S.Stmt -> Either String S.Stmt
transpileStatement ctx = runCompiler ctx . tStatement

runCompiler :: CContext -> Compiler a -> Either String a
runCompiler context transpiler = runExcept $ runReaderT transpiler context
