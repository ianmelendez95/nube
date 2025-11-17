module Nube.Compiler
  ( Compiler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (MonadState, modify)
import Data.Text qualified as T
import Nube.Context (NContext (..))

type Compiler a = ReaderT NContext (Except String) a
