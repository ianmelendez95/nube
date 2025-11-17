module Nube.Compiler
  ( Compiler,
  )
where

import Control.Monad.Except ( Except )
import Control.Monad.Reader ( ReaderT )
import Nube.Context (NContext (..))

type Compiler a = ReaderT NContext (Except String) a
