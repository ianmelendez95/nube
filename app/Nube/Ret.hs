module Nube.Ret where

import Control.Lens (Traversal')
import Control.Monad.Except (throwError)
import Nube.Compiler (Compiler)
import Nube.Syntax qualified as S

compileReturns :: S.Script -> Compiler S.Script
compileReturns = scriptFnStmts' cStmts

cStmts :: [S.Stmt] -> Compiler [S.Stmt]
cStmts [] = pure []
cStmts [e] =
  case e of
    (S.SReturn _) -> pure [e]
    (S.SExpr e') -> pure [S.SReturn e']
    _ -> throwError $ "Last statement not an expression or return statement: " ++ show e
cStmts (e : es) = (e :) <$> cStmts es

scriptFnStmts' :: Traversal' S.Script [S.Stmt]
scriptFnStmts' = S.scriptFns' . traverse . S.fnStmts'