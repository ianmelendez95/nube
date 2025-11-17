module Nube.Context
  ( NContext (..),
    ctxAddFnM,
    ctxAddFn,
    ctxGetIsFnM,
    ctxIsFn,
    ctxAskIsFn,
  )
where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, gets, modify)
import Data.Text qualified as T

newtype NContext = NContext
  { fnNames :: [T.Text]
  }
  deriving (Show, Eq)

ctxAddFnM :: (MonadState NContext m) => T.Text -> m ()
ctxAddFnM = modify . ctxAddFn

ctxGetIsFnM :: (MonadState NContext m) => T.Text -> m Bool
ctxGetIsFnM = gets . ctxIsFn

ctxAskIsFn :: (MonadReader NContext m) => T.Text -> m Bool
ctxAskIsFn = asks . ctxIsFn

ctxAddFn :: T.Text -> NContext -> NContext
ctxAddFn fn_name (NContext fn_names) = NContext (fn_name : fn_names)

ctxIsFn :: T.Text -> NContext -> Bool
ctxIsFn fn_name (NContext fn) = fn_name `elem` fn