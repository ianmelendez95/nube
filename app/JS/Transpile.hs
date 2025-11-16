module JS.Transpile
  ( Transpiler,
    TContext (..),
    ContSplit (..),
    ctx_var_name,
    transpileStatement,
    splitStmtContinuations,
    runTranspiler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.List.Split
import Data.Text qualified as T
import JS.Syntax qualified as S

-- import Polysemy
--   ( Sem,
--     run,
--   )
-- import Polysemy.Error
--   ( Error,
--     runError,
--     throw,
--   )
-- import Polysemy.Reader
--   ( Reader,
--     asks,
--     runReader,
--   )

data Cont = Cont [S.Stmt]

data TContext = TContext
  { fnNames :: [T.Text]
  }

-- type Transpiler a = Sem '[Reader TContext, Error String] a

type Transpiler a = ReaderT TContext (Except String) a

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

splitStmtContinuations :: [S.Stmt] -> Transpiler [ContSplit]
splitStmtContinuations stmts = do
  -- stmts_with_fn_calls :: [(S.Stmt, [T.Text])]
  cont_splits <- mapM stmtToContSplit stmts
  pure $ concatContSplits cont_splits
  where
    splitOnFnCalls :: [(S.Stmt, [T.Text])] -> [[(S.Stmt, [T.Text])]]
    splitOnFnCalls = split . dropInnerBlanks . whenElt $ (not . null . snd)

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
spanMaybe m_fn (x : xs) =
  case m_fn x of
    Just y ->
      let (ys, rest) = spanMaybe m_fn xs
       in (y : ys, rest)
    Nothing -> ([], xs)
spanMaybe _ [] = ([], [])

stmtToContSplit :: S.Stmt -> Transpiler ContSplit
stmtToContSplit (S.SConst var (S.ECall (S.EVar fn_name) fn_args)) =
  pure $ ContCall var fn_name fn_args
stmtToContSplit stmt = do
  fn_calls <- userFnCallsInStmt stmt
  if null fn_calls
    then pure $ ContBlock [stmt]
    else throwError $ "Can only call user-defined functions in a simple const assignment call: " ++ show stmt

userFnCallsInStmt :: S.Stmt -> Transpiler [T.Text]
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

userFnCallsInExpr :: S.Expr -> Transpiler [T.Text]
userFnCallsInExpr (S.EVar v) = do
  is_ufn <- isUserFn v
  if is_ufn then pure [v] else pure []
userFnCallsInExpr (S.ECall lhs args) = userFnCallsInExprs (lhs : args)
userFnCallsInExpr (S.EMember lhs (S.MBracketAccess rhs)) = userFnCallsInExprs [lhs, rhs]
userFnCallsInExpr (S.EMember lhs _) = userFnCallsInExpr lhs
userFnCallsInExpr (S.EInfix _ lhs rhs) = userFnCallsInExprs [lhs, rhs]
userFnCallsInExpr (S.EStringLit _) = pure []
userFnCallsInExpr (S.ENumberLit _) = pure []

userFnCallsInExprs :: [S.Expr] -> Transpiler [T.Text]
userFnCallsInExprs es = concat <$> traverse userFnCallsInExpr es

--   S.EMember <$> userFnCallsInExpr lhs <*> pure dotAccess
--   S.EInfix op <$> userFnCallsInExpr lhs <*> userFnCallsInExpr rhs
-- userFnCallsInExpr e = pure e

isUserFn :: T.Text -> Transpiler Bool
isUserFn name = asks ((name `elem`) . fnNames)

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

ctxAssignArgStmt :: T.Text -> Int -> S.Stmt
ctxAssignArgStmt var_name arg_idx =
  S.SAssign (ctxFrameVar var_name) (ctxArg arg_idx)

ctxCallStmt :: T.Text -> [S.Expr] -> T.Text -> S.Stmt
ctxCallStmt fn_name args cont_name =
  S.SExpr (S.ECall (S.dotMemberExpr ctx_var_name fn_name) [S.EVar fn_name, S.EListLit args, S.EVar cont_name])

ctxFrameVar :: T.Text -> S.Expr
ctxFrameVar v = S.dotMembers ctx_var_name ["frame", v]

ctxArg :: Int -> S.Expr
ctxArg arg_idx = S.mkBracketMember (ctxDotMember "args") (S.ENumberLit arg_idx)

ctxDotMember :: T.Text -> S.Expr
ctxDotMember = S.dotMemberExpr ctx_var_name

ctx_var_name :: S.Expr
ctx_var_name = S.EVar "_ctx"