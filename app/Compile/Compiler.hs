module Compile.Compiler
  ( TContext (..),
    Compiler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T

newtype TContext = TContext
  { fnNames :: [T.Text]
  }

type Compiler a = ReaderT TContext (Except String) a