{-# LANGUAGE OverloadedStrings #-}


module JS.Syntax where 

import qualified Data.Text as T

data Fun = Fun T.Text T.Text T.Text -- name params body

funName :: Fun -> T.Text
funName (Fun n _ _) = T.strip n

instance Show Fun where 
  show (Fun name params body) = 
    T.unpack $ "async function " <> name <> params <> body
