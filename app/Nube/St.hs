module Nube.St 
  (compileScriptSt) where  

import Nube.Syntax qualified as S
import Nube.Compiler (Compiler)

import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (first)
import Data.Text qualified as T
import Nube.Compiler (Compiler)
import Nube.Context (ctxAskIsFn)
import Nube.JSCtx (ctxAssignArgStmt, ctxCallStmt, ctx_var_text)

data StateSplit
  = StateBlock [S.Stmt]
  | StateCall
      { _contCallVar :: T.Text,
        _contCallFn :: T.Text,
        _contCallArgs :: [S.Expr]
      }
  deriving (Show)

compileScriptSt :: S.Script -> IO S.Script
compileScriptSt = undefined

compileStmts :: [S.Stmt] -> [S.Stmt]
compileStmts = undefined

-- splitStateInScript :: S.Script -> Compiler S.Script
-- splitStateInScript (S.Script s_name s_fns) = S.Script s_name . concat <$> mapM splitFnState s_fns

-- splitFnStateinuations :: S.Fn -> Compiler [S.Fn]
-- splitFnStateinuations fn@(S.Fn _ _ []) = pure [fn]
-- splitFnStateinuations (S.Fn fn_name fn_params fn_stmts) = do
--   split_blocks <- splitStmtStateinuations fn_name fn_stmts
--   case split_blocks of
--     [] -> throwError "Stateinuation splitting returned empty result"
--     (first_block : rest_blocks) ->
--       let arg_stmts = zipWith ctxAssignArgStmt fn_params [0 ..]
--           primary_fn = S.Fn fn_name [ctx_var_text] (arg_stmts ++ first_block)
--           cont_fns = zipWith mkFnFromBlock [1 ..] rest_blocks
--        in pure $ primary_fn : cont_fns
--   where
--     mkFnFromBlock :: Int -> [S.Stmt] -> S.Fn
--     mkFnFromBlock cont_num = S.Fn (fn_name <> "C" <> T.show cont_num) [ctx_var_text]

-- splitStmtStates :: [S.Stmt] -> Compiler [[S.Stmt]]
-- splitStmtStates stmts = do
--   cont_splits <- mapM stmtToStateSplit stmts
--   pure $ concatStateSplits 0 cont_splits

concatStateSplits :: T.Text -> Int -> [StateSplit] -> [[S.Stmt]]
concatStateSplits fn_name cont_num splits =
  case spanBlockStmts splits of
    ([], []) -> []
    (block_stmts, []) -> [block_stmts]
    (block_stmts, StateCall var call_fn_name call_fn_args : rest_splits) ->
      let -- call the function with the next continuation
          cont_call :: S.Stmt
          cont_call = ctxCallStmt call_fn_name call_fn_args (fn_name <> "C" <> T.show (cont_num + 1))

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

