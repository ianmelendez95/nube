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
  tplStage  :: Named RStage,
  tplFuns   :: [Named RFun]
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

-- | Lambda Function Resource
data RFun = RFun {
  funName :: T.Text,
  funApi  :: Ref
}

newtype Ref = Ref { refId :: T.Text }

newtype Sub = Sub T.Text

instance ToJSON Template where 
  toJSON (Template params api stage funs) = object 
    [ "Parameters" .= object (map namedKV params)
    , "Resources"  .= object (
        [ namedKV api
        , namedKV stage
        ] ++ map namedKV funs
      )
    ]

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

instance ToJSON RStage where 
  toJSON (RStage api) = 
    object [ "Type" .= fromText "AWS::ApiGatewayV2::Stage" 
           , "Properties" .= object 
               [ "ApiId"      .= api 
               , "AutoDeploy" .= True
               , "StageName"  .= fromText "$default"
               ]
           ]

instance ToJSON RFun where 
  toJSON (RFun name api) = object 
    [ "Type" .= fromText "AWS::Lambda::Function" 
    , "Properties" .= object 
        [ "Code" .= object 
            [ "S3Bucket" .= test_bucket_ref
            , "S3Key"    .= fromText (name <> "-code.zip")
            ]
        , "Environment" .= object 
            [ "Variables" .= object 
                [ "AWS_GATEWAY_HOST" .= hostFromApiRef api
                ]
            ]
        , "FunctionName" .= name
        , "Handler" .= fromText (name <> ".handler")
        , "Role" .= test_role_ref
        , "Runtime" .= fromText "nodejs16.x"
        ]
    ]

instance ToJSON Ref where 
  toJSON (Ref ref_id) = object [ "Ref" .= fromText ref_id ]

instance ToJSON Sub where 
  toJSON (Sub ref_id) = object [ "Fn::Sub" .= fromText ref_id ]

templateFromScript :: S.Script -> Template
templateFromScript script = 
  let name  = S.scriptName script
      api   = apiFromScriptName name
      stage = stageFromApi api
      funs  = map (jsFunToLambda (namedRef api)) (S.scriptFuns script)
   in Template test_params api stage funs

jsFunToLambda :: Ref -> S.Fun -> Named RFun
jsFunToLambda api_ref fun = 
  let name = S.funName fun
   in Named (capitalizeFirst name <> "Function") $ RFun name api_ref

-- API

apiFromScriptName :: T.Text -> Named RApi
apiFromScriptName name = Named (capitalizeFirst name <> "Api") (RApi (name <> "-api")) 

stageFromApi :: Named RApi -> Named RStage
stageFromApi api = 
  let api_name = namedName api
   in Named (api_name <> "Stage") $ RStage (Ref api_name)
  
-- | name here is the resource name/logical id
hostFromApiRef :: Ref -> Sub
hostFromApiRef ref = 
  Sub $ "${" <> refId ref <> "}.execute-api.${AWS::Region}.amazonaws.com"

-- Named

namedKV :: ToJSON a => Named a -> (Key, Value)
namedKV (Named name value) = (fromText name, toJSON value)

namedRef :: Named a -> Ref
namedRef = Ref . namedName

-- Text 

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst txt = 
  case T.uncons txt of 
    Nothing -> txt
    Just (c, cs) -> toUpper c `T.cons` cs

-- Test Misc.

test_params :: [Named Param]
test_params = 
  [ Named (refId test_bucket_ref) $ Param "capitalizewords-bucket"
  , Named (refId test_role_ref)   $ Param "arn:aws:iam::565652071321:role/service-role/hello-aws-role-aoys6qk7"
  ]

test_bucket_ref :: Ref
test_bucket_ref = Ref "CapitalizeWordsBucket"

test_role_ref :: Ref
test_role_ref = Ref "CapitalizeWordsRole"
 