module Nube.ToJS
  ( ToJS, 
    toJS,
  )
where

import Data.Text qualified as T
import Data.Text (Text)

class ToJS a where 
  toJS :: a -> Text


