module Compile.Compiler
  ( CContext (..),
    Compiler,
    isUserFn,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T

newtype CContext = CContext
  { fnNames :: [T.Text]
  }

type Compiler a = ReaderT CContext (Except String) a

isUserFn :: T.Text -> Compiler Bool
isUserFn name = asks ((name `elem`) . fnNames)