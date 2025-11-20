module Nube.Ret where

import Control.Lens (Setter')
import Nube.Compiler (Compiler)
import Nube.Syntax qualified as S

compileReturns :: S.Script -> Compiler S.Script
compileReturns = undefined

cStmts :: [S.Stmt] -> Compiler [S.Stmt]
cStmts = undefined

scriptFnStmts' :: Setter' S.Script [S.Stmt]
scriptFnStmts' = S.scriptFns' . traverse . S.fnStmts'