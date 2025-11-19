module Nube.Compiler
  ( Compiler,
    CompilerT,
    runCompiler,
    runCompilerT
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Functor.Identity (runIdentity)
import Nube.Context (NContext (..))

type Compiler a = CompilerT Identity a

type CompilerT m a = ReaderT NContext (ExceptT String m) a

runCompiler :: NContext -> Compiler a -> Either String a
runCompiler context = runIdentity . runCompilerT context

runCompilerT :: (Monad m) => NContext -> CompilerT m a -> m (Either String a)
runCompilerT context compiler_t = runExceptT $ runReaderT compiler_t context