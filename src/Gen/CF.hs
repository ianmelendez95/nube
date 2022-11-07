{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen.CF where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Aeson
import Data.Aeson.Key
import qualified Data.Aeson.Encode.Pretty as JSONPretty
import Data.String (fromString)
import Data.Char

import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Map (Map)
import qualified Data.Map as Map

import qualified JS.Syntax as S

data Named a = Named {
  namedName :: T.Text,
  namedItem :: a
}

newtype Param = Param {
  paramDefault :: T.Text
}

newtype RApi = RApi {
  apiName :: T.Text
}

instance ToJSON Param where 
  toJSON (Param def) = object [ "Type" .= fromText "String", "Default" .= def ]

instance ToJSON RApi where 
  toJSON (RApi name) =
    object [ "Type" .= fromText "AWS::ApiGatewayV2::Api" 
           , "Properties" .= object 
               [ "Name" .= name
               , "ProtocolType" .= fromText "HTTP"
               ]
           ]

templateFromScript :: S.Script -> T.Text
templateFromScript script = 
  let name = S.scriptName script
      api  = apiFromScriptName name
   in encodePretty (mkCfTemplate test_params api)

mkCfTemplate :: [Named Param] -> Named RApi -> Value
mkCfTemplate params api = object 
  [ "Parameters" .= object (map namedKV params)
  , "Resources" .= object 
      [ namedKV api ]
  ]

-- API

apiFromScriptName :: T.Text -> Named RApi
apiFromScriptName name = Named (capitalizeFirst name <> "Api") (RApi (name <> "-api")) 

-- Named

namedKV :: ToJSON a => Named a -> (Key, Value)
namedKV (Named name value) = (fromText name, toJSON value)

-- Text 

encodePretty :: ToJSON a => a -> T.Text
encodePretty = decodeUtf8 . BS.toStrict . JSONPretty.encodePretty

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst txt = 
  case T.uncons txt of 
    Nothing -> txt
    Just (c, cs) -> toUpper c `T.cons` cs

-- Test Misc.

test_params :: [Named Param]
test_params = 
  [ Named "CapitalizeWordsBucket" $ Param "capitalizewords-bucket"
  , Named "CapitalizeWordsRole"   $ Param "arn:aws:iam::565652071321:role/service-role/hello-aws-role-aoys6qk7"
  ]

test_api_res :: Named RApi
test_api_res = Named "CapitalizeWordsApi" (RApi "capitalizeWords-api")

-- test_gen :: IO ()
-- test_gen = do 
--   let temp = mkCfTemplate test_params
--   TIO.putStrLn (encodePretty temp)
 