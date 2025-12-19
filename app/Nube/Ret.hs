module Nube.Ret where

import Control.Lens (Traversal', (%%~))
import Control.Monad.Except (throwError)
import Nube.Compiler (Compiler)
import Nube.Syntax qualified as S
import Nube.Syntax
import Data.List (singleton)

compileReturns :: S.Script -> Compiler S.Script
compileReturns = scriptFnStmts' cStmts

scriptFnStmts' :: Traversal' S.Script [S.Stmt]
scriptFnStmts' = S.scriptFns' . traverse . S.fnStmts'

cStmts :: [S.Stmt] -> Compiler [S.Stmt]
cStmts [] = pure []
cStmts [e] =
  case e of
    (SReturn _) -> pure [e]
    (SExpr e') -> pure [S.SReturn e']
    (SSwitch s_match s_cases) -> 
      singleton . SSwitch s_match <$> mapM (S.caseStmts' %%~ cStmts) s_cases
    _ -> throwError $ "Last statement not a switch statement: " ++ show e
cStmts (e : es) = (e :) <$> cStmts es



