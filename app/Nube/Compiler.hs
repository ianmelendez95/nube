module Nube.Compiler
  ( Compiler,
    isUserFn,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (MonadState, modify)
import Data.Text qualified as T
import Nube.Context (NContext (..))

type Compiler a = ReaderT NContext (Except String) a

isUserFn :: T.Text -> Compiler Bool
isUserFn name = asks ((name `elem`) . fnNames)
