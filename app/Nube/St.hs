module Nube.St
  ( compileScriptSt,
    splitFnStates,
  )
where

import Control.Lens ((%%~))
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (first)
import Data.Text qualified as T
import Nube.Compiler (Compiler)
import Nube.Context (ctxAskIsFn)
import Nube.JSCtx (ctxAssignArgStmt, ctxCallStmt, ctxDotMember, ctx_var_text)
import Nube.Syntax qualified as S

data StateSplit
  = StateBlock [S.Stmt]
  | StateCall
      { _contCallVar :: T.Text,
        _contCallFn :: T.Text,
        _contCallArgs :: [S.Expr]
      }
  deriving (Show)

compileScriptSt :: S.Script -> Compiler S.Script
compileScriptSt = S.scriptFns' %%~ traverse splitFnStates

splitFnStates :: S.Fn -> Compiler S.Fn
splitFnStates fn@(S.Fn _ _ []) = pure fn
splitFnStates (S.Fn fn_name fn_params fn_stmts) = do
  blocks <- splitStmtStates fn_name fn_stmts
  let arg_stmts = zipWith ctxAssignArgStmt fn_params [0 ..]
      cont_cases = zipWith S.SCase [1 ..] blocks
      state_switch = S.SSwitch (ctxDotMember "state") cont_cases
  pure $ S.Fn fn_name [ctx_var_text] (arg_stmts ++ [state_switch])

splitStmtStates :: T.Text -> [S.Stmt] -> Compiler [[S.Stmt]]
splitStmtStates fn_name stmts = do
  cont_splits <- mapM stmtToStateSplit stmts
  pure $ concatStateSplits fn_name 0 cont_splits

concatStateSplits :: T.Text -> Int -> [StateSplit] -> [[S.Stmt]]
concatStateSplits fn_name cont_num splits =
  case spanBlockStmts splits of
    ([], []) -> []
    (block_stmts, []) -> [block_stmts]
    (block_stmts, StateCall var call_fn_name call_fn_args : rest_splits) ->
      let -- call the function with the next continuation
          cont_call :: S.Stmt
          cont_call = ctxCallStmt call_fn_name call_fn_args fn_name (cont_num + 1)

          -- the final current block, with the call with continuation at the end
          block' :: [S.Stmt]
          block' = block_stmts ++ [cont_call]

          cont_result_block :: StateSplit
          cont_result_block = StateBlock [ctxAssignArgStmt var 0]
       in block' : concatStateSplits fn_name (cont_num + 1) (cont_result_block : rest_splits)
    -- TODO - improve span approach
    (_, StateBlock {} : _) -> error "spanBlockStmts did not chunk correctly"
  where
    spanBlockStmts :: [StateSplit] -> ([S.Stmt], [StateSplit])
    spanBlockStmts = first concat . spanMaybe maybeBlock

    maybeBlock :: StateSplit -> Maybe [S.Stmt]
    maybeBlock (StateBlock stmts) = Just stmts
    maybeBlock _ = Nothing

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe m_fn rest@(x : xs) =
  case m_fn x of
    Just y ->
      let (ys, rest') = spanMaybe m_fn xs
       in (y : ys, rest')
    Nothing -> ([], rest)
spanMaybe _ [] = ([], [])

stmtToStateSplit :: S.Stmt -> Compiler StateSplit
stmtToStateSplit (S.SConst var (S.ECall (S.EVar fn_name) fn_args)) =
  pure $ StateCall var fn_name fn_args
stmtToStateSplit stmt = do
  fn_calls <- userFnCallsInStmt stmt
  if null fn_calls
    then pure $ StateBlock [stmt]
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
userFnCallsInStmt (S.SSwitch{}) = throwError "Switch statements not allowed"
userFnCallsInStmt S.SBreak = throwError "Break statements not allowed"

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
