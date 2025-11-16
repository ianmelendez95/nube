module Compile.Compiler
  ( TContext (..),
    Transpiler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T

newtype TContext = TContext
  { fnNames :: [T.Text]
  }

type Transpiler a = ReaderT TContext (Except String) a