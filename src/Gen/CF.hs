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

import Util.Aeson

import qualified JS.Syntax as S

data Named a = Named {
  namedName :: T.Text,
  namedItem :: a
}

data Template = Template {
  tplParams :: [Named Param],
  tplApi    :: Named RApi,
  tplStage  :: Named RStage
}

newtype Param = Param {
  paramDefault :: T.Text
}

newtype RApi = RApi {
  apiName :: T.Text
}

newtype RStage = RStage {
  stageApi:: Ref
}

newtype Ref = Ref T.Text

instance ToJSON Template where 
  toJSON (Template params api stage) = object 
    [ "Parameters" .= object (map namedKV params)
    , "Resources"  .= object 
        [ namedKV api
        , namedKV stage
        ]
    ]

instance ToJSON Param where 
  toJSON (Param def) = object [ "Type" .= fromText "String", "Default" .= def ]

instance ToJSON Ref where 
  toJSON (Ref ref_id) = object [ "Ref" .= fromText ref_id ]

instance ToJSON RApi where 
  toJSON (RApi name) =
    object [ "Type" .= fromText "AWS::ApiGatewayV2::Api" 
           , "Properties" .= object 
               [ "Name" .= name
               , "ProtocolType" .= fromText "HTTP"
               ]
           ]

instance ToJSON RStage where 
  toJSON (RStage api) = 
    object [ "Type" .= fromText "AWS::ApiGatewayV2::Stage" 
           , "Properties" .= object 
               [ "ApiId"      .= toJSON api 
               , "AutoDeploy" .= True
               , "StageName"  .= fromText "$default"
               ]
           ]

templateFromScript :: S.Script -> Template
templateFromScript script = 
  let name  = S.scriptName script
      api   = apiFromScriptName name
      stage = stageFromApi api
   in Template test_params api stage

mkCfTemplate :: [Named Param] 
             -> Named RApi 
             -> Value
mkCfTemplate params api = object 
  [ "Parameters" .= object (map namedKV params)
  , "Resources" .= object 
      [ namedKV api ]
  ]

-- API

apiFromScriptName :: T.Text -> Named RApi
apiFromScriptName name = Named (capitalizeFirst name <> "Api") (RApi (name <> "-api")) 

stageFromApi :: Named RApi -> Named RStage
stageFromApi api = 
  let api_name = namedName api
   in Named (api_name <> "Stage") $ RStage (Ref api_name)

-- Named

namedKV :: ToJSON a => Named a -> (Key, Value)
namedKV (Named name value) = (fromText name, toJSON value)

-- Text 

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
 