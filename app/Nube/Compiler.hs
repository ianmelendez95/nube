module Nube.Compiler
  ( CContext (..),
    Compiler,
    isUserFn,
    ctxAddFnM,
    ctxAddFn,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (MonadState, modify)
import Data.Text qualified as T

newtype CContext = CContext
  { fnNames :: [T.Text]
  }
  deriving (Show, Eq)

type Compiler a = ReaderT CContext (Except String) a

isUserFn :: T.Text -> Compiler Bool
isUserFn name = asks ((name `elem`) . fnNames)

ctxAddFnM :: (MonadState CContext m) => T.Text -> m ()
ctxAddFnM fn_name = modify (ctxAddFn fn_name)

ctxAddFn :: T.Text -> CContext -> CContext
ctxAddFn fn_name (CContext fnNames) = CContext (fn_name : fnNames)