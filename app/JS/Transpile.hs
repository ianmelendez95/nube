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

runTranspiler :: TContext -> Transpiler a -> Either String a
runTranspiler context transpiler = runExcept $ runReaderT transpiler context

transpileSem :: S.Expr -> Transpiler S.Expr
transpileSem e@(S.EVar v) = do
  fn_names <- asks fnNames
  if elem v fn_names
    then throwError "fn exists"
    else pure e

splitStmtContinuations :: [S.Stmt] -> [Cont]
splitStmtContinuations stmts = undefined

transpileStatement :: S.Stmt -> Either String S.Stmt
transpileStatement (S.SReturn e) = transpileReturn e
transpileStatement (S.SConst var rhs) = transpileAssign var rhs
transpileStatement (S.SAssign _ _) = Left "Reassignment is not allowed, use a new const var"
transpileStatement (S.SExpr _) = Left "Expression statements are not allowed"

transpileExpr :: S.Expr -> Either String S.Expr
transpileExpr (S.EVar v) = transpileVar v
transpileExpr (S.ECall lhs args) =
  S.ECall <$> transpileExpr lhs <*> traverse transpileExpr args
transpileExpr (S.EMember lhs (S.MBracketAccess rhs)) =
  S.EMember <$> transpileExpr lhs <*> (S.MBracketAccess <$> transpileExpr rhs)
transpileExpr (S.EMember lhs dotAccess) =
  S.EMember <$> transpileExpr lhs <*> pure dotAccess
transpileExpr (S.EInfix op lhs rhs) =
  S.EInfix op <$> transpileExpr lhs <*> transpileExpr rhs
transpileExpr e = Right e

transpileAssign :: T.Text -> S.Expr -> Either String S.Stmt
transpileAssign var rhs = S.SAssign <$> transpileVar var <*> Right rhs

transpileVar :: T.Text -> Either String S.Expr
transpileVar v = Right $ ctxFrameVar v

transpileReturn :: S.Expr -> Either String S.Stmt
transpileReturn e = do
  e' <- transpileExpr e
  pure $ S.SExpr (S.ECall (ctxDotMember "return") [e'])

ctxFrameVar :: T.Text -> S.Expr
ctxFrameVar v = S.dotMembers ctx_var_name ["frame", v]

ctxDotMember :: T.Text -> S.Expr
ctxDotMember = S.dotMemberExpr ctx_var_name

ctx_var_name :: S.Expr
ctx_var_name = S.EVar "_ctx"