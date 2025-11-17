module Nube.Cont
  ( ContSplit (..),
    splitStmtContinuations,
  )
where

-- import Debug.Trace (trace, traceShowId)

import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (first)
import Data.Text qualified as T
import Nube.Compiler (Compiler)
import Nube.Context (ctxAskIsFn)
import Nube.JSCtx (ctxAssignArgStmt, ctxCallStmt)
import Nube.Syntax qualified as S

data ContSplit
  = ContBlock [S.Stmt]
  | ContCall
      { _contCallVar :: T.Text,
        _contCallFn :: T.Text,
        _contCallArgs :: [S.Expr]
      }
  deriving (Show)

splitStmtContinuations :: [S.Stmt] -> Compiler [ContSplit]
splitStmtContinuations stmts = do
  -- stmts_with_fn_calls :: [(S.Stmt, [T.Text])]
  cont_splits <- mapM stmtToContSplit stmts
  pure $ concatContSplits cont_splits

concatContSplits :: [ContSplit] -> [ContSplit]
concatContSplits splits =
  case spanBlockStmts splits of
    ([], []) -> []
    (block_stmts, []) -> [ContBlock block_stmts]
    (block_stmts, ContCall var fn_name fn_args : rest_splits) ->
      let cont_call :: S.Stmt
          cont_call = ctxCallStmt fn_name fn_args "__test_continuation__"

          block' :: ContSplit
          block' = ContBlock (block_stmts ++ [cont_call])

          cont_result_block :: ContSplit
          cont_result_block = ContBlock [ctxAssignArgStmt var 0]
       in block' : concatContSplits (cont_result_block : rest_splits)
    -- TODO - improve span approach
    (_, ContBlock {} : _) -> error "spanBlockStmts did not chunk correctly"
  where
    spanBlockStmts :: [ContSplit] -> ([S.Stmt], [ContSplit])
    spanBlockStmts = first concat . spanMaybe maybeBlock

    maybeBlock :: ContSplit -> Maybe [S.Stmt]
    maybeBlock (ContBlock stmts) = Just stmts
    maybeBlock _ = Nothing

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe m_fn rest@(x : xs) =
  case m_fn x of
    Just y ->
      let (ys, rest') = spanMaybe m_fn xs
       in (y : ys, rest')
    Nothing -> ([], rest)
spanMaybe _ [] = ([], [])

stmtToContSplit :: S.Stmt -> Compiler ContSplit
stmtToContSplit (S.SConst var (S.ECall (S.EVar fn_name) fn_args)) =
  pure $ ContCall var fn_name fn_args
stmtToContSplit stmt = do
  fn_calls <- userFnCallsInStmt stmt
  if null fn_calls
    then pure $ ContBlock [stmt]
    else throwError $ "Can only call user-defined functions in a simple const assignment call: " ++ show stmt

userFnCallsInStmt :: S.Stmt -> Compiler [T.Text]
userFnCallsInStmt e@(S.SConst _ rhs) = do
  fn_calls <- userFnCallsInExpr rhs
  if null fn_calls
    then pure []
    else
      if length fn_calls > 1
        then throwError $ "Cannot call multiple user-defined functions in a const assignment: " ++ show fn_calls
        else case rhs of
          (S.ECall (S.EVar _) _) -> pure fn_calls
          _ -> throwError $ "Calls to user-defined functions must be a simple fn(args) call: " ++ show e
userFnCallsInStmt e@(S.SReturn rhs) = do
  fn_calls <- userFnCallsInExpr rhs
  if not . null $ fn_calls
    then throwError $ "Cannot call user-defined functions in a return statement: " ++ show e
    else pure fn_calls
userFnCallsInStmt (S.SAssign _ _) = throwError "Reassignment is not allowed, use a new const var"
userFnCallsInStmt (S.SExpr _) = throwError "Expression statements are not allowed"

userFnCallsInExpr :: S.Expr -> Compiler [T.Text]
userFnCallsInExpr (S.EVar v) = do
  is_ufn <- ctxAskIsFn v
  if is_ufn then pure [v] else pure []
userFnCallsInExpr (S.ECall lhs args) = userFnCallsInExprs (lhs : args)
userFnCallsInExpr (S.EMember lhs (S.MBracketAccess rhs)) = userFnCallsInExprs [lhs, rhs]
userFnCallsInExpr (S.EMember lhs _) = userFnCallsInExpr lhs
userFnCallsInExpr (S.EInfix _ lhs rhs) = userFnCallsInExprs [lhs, rhs]
userFnCallsInExpr (S.EStringLit _) = pure []
userFnCallsInExpr (S.ENumberLit _) = pure []
userFnCallsInExpr (S.EListLit es) =
  concat <$> traverse userFnCallsInExpr es

userFnCallsInExprs :: [S.Expr] -> Compiler [T.Text]
userFnCallsInExprs es = concat <$> traverse userFnCallsInExpr es
