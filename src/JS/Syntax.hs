{-# LANGUAGE OverloadedStrings #-}


module JS.Syntax where 

import qualified Data.Text as T

data Fun = Fun { funName   :: T.Text 
               , funParams :: T.Text 
               , funBody   :: T.Text
               }

instance Show Fun where 
  show = T.unpack . funText

funText :: Fun -> T.Text
funText (Fun name params body) = "async function " <> name <> params <> body
