module Nube.Compiler
  ( Compiler,
  )
where

import Control.Monad.Except (Except, ExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT)
import Nube.Context (NContext (..))

type Compiler a = CompilerT Identity a

type CompilerT m a = ReaderT NContext (ExceptT String m) a
