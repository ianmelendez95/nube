module Compile.Compiler
  ( CContext (..),
    Compiler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T

newtype CContext = CContext
  { fnNames :: [T.Text]
  }

type Compiler a = ReaderT CContext (Except String) a