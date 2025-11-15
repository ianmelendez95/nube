module JS.Transpile
  ( Transpiler,
    TContext (..),
    ctx_var_name,
    transpileStatement,
    splitStmtContinuations,
    runTranspiler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
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

transpileScript :: S.Script -> Either String S.Script
transpileScript (S.Script name fns) =
  let ctx = TContext (map S.fnName fns)
   in runTranspiler ctx $ do
        fns' <- mconcat <$> mapM transpileFn fns
        pure $ S.Script name fns'

transpileFn :: S.Fn -> Transpiler [S.Fn]
transpileFn (S.Fn name params stmts) = undefined

splitStmtContinuations :: [S.Stmt] -> Transpiler [[(S.Stmt, [T.Text])]]
splitStmtContinuations stmts = do
  -- stmts_with_fn_calls :: [(S.Stmt, [T.Text])]
  stmts_with_fn_calls <- mapM (\stmt -> (stmt,) <$> userFnCallsInStmt stmt) stmts
  pure $ splitOnFnCalls stmts_with_fn_calls
  where
    splitOnFnCalls :: [(S.Stmt, [T.Text])] -> [[(S.Stmt, [T.Text])]]
    splitOnFnCalls = split . dropInnerBlanks . whenElt $ (not . null . snd)

-- splitStmtsOnFnCalls :: [S.Stmt] -> Transpiler [([S.Stmt], S.Stmt)]
-- splitStmtsOnFnCalls stmts = _

userFnCallsInStmt :: S.Stmt -> Transpiler [T.Text]
userFnCallsInStmt (S.SConst _ rhs) = do
  fn_calls <- userFnCallsInExpr rhs
  if length fn_calls > 1
    then throwError $ "Cannot call multiple user-defined functions in a const assignment: " ++ show fn_calls
    else pure fn_calls
userFnCallsInStmt (S.SReturn rhs) = userFnCallsInExpr rhs
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

ctxFrameVar :: T.Text -> S.Expr
ctxFrameVar v = S.dotMembers ctx_var_name ["frame", v]

ctxDotMember :: T.Text -> S.Expr
ctxDotMember = S.dotMemberExpr ctx_var_name

ctx_var_name :: S.Expr
ctx_var_name = S.EVar "_ctx"