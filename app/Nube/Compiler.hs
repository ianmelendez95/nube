module Nube.Compiler
  ( Compiler,
    IOCompiler,
    runCompiler,
  )
where

import Control.Monad.Except (ExceptT, runExcept, runExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT, runReaderT)
import Nube.Context (NContext (..))

type Compiler a = CompilerT Identity a

type IOCompiler a = CompilerT IO a

type CompilerT m a = ReaderT NContext (ExceptT String m) a

runCompiler :: NContext -> Compiler a -> Either String a
runCompiler context transpiler = runExcept $ runReaderT transpiler context

runCompilerT :: (Monad m) => NContext -> CompilerT m a -> m (Either String a)
runCompilerT context compiler_t = runExceptT $ runReaderT compiler_t context