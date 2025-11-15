module JS.Transpile
  ( TContext (..),
    transpileSem,
    ctx_var_name,
    transpileStatement,
    splitStmtContinuations,
    runTranspiler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
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

transpileScript :: S.Script -> S.Script
transpileScript (S.Script name fns) = _

transpileSem :: S.Expr -> Transpiler S.Expr
transpileSem e@(S.EVar v) = do
  fn_names <- asks fnNames
  if v `elem` fn_names
    then throwError "fn exists"
    else pure e
transpileSem _ = undefined

splitStmtContinuations :: [S.Stmt] -> [Cont]
splitStmtContinuations stmts = undefined

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