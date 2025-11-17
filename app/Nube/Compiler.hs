module Nube.Compiler
  ( CContext (..),
    Compiler,
    isUserFn,
    ctxAddFn,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T

newtype CContext = CContext
  { fnNames :: [T.Text]
  }
  deriving (Show, Eq)

type Compiler a = ReaderT CContext (Except String) a

isUserFn :: T.Text -> Compiler Bool
isUserFn name = asks ((name `elem`) . fnNames)

ctxAddFn :: T.Text -> CContext -> CContext
ctxAddFn fn_name (CContext fnNames) = CContext (fn_name : fnNames)