{-# LANGUAGE OverloadedStrings #-}


module JS.Syntax where 

import qualified Data.Text as T

data Fun = Fun T.Text T.Text T.Text -- name params body

instance Show Fun where 
  show (Fun name params body) = 
    T.unpack $ "async function " <> name <> params <> body
