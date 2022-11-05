{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen.CF where 

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Encode.Pretty
import Data.String (fromString)

import qualified Data.ByteString.Lazy.Char8 as BS

data Param = Param {
  paramName    :: T.Text,
  paramDefault :: T.Text
}

mkCfTemplate :: [Param] -> Value
mkCfTemplate params = object [paramsKV params]

paramsKV :: [Param] -> (Key, Value)
paramsKV params = 
  "Parameters" .= object (map paramKV params)

paramKV :: Param -> (Key, Value)
paramKV (Param name def) =
  let body :: Value
      body = object [ "Type" .= fromText "String", "Default" .= def ]
   in fromText name .= body

test_params :: [Param]
test_params = 
  [ Param "CapitalizeWordsBucket" "capitalizewords-bucket"
  , Param "CapitalizeWordsRole"   "arn:aws:iam::565652071321:role/service-role/hello-aws-role-aoys6qk7"
  ]

test_gen :: IO ()
test_gen = do 
  let temp = mkCfTemplate test_params
  BS.putStrLn (encodePretty temp)
 