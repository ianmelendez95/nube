{-# LANGUAGE OverloadedStrings #-}

module Gen.CF where 

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Key
import Data.Char

import qualified JS.Syntax as S

data Named a = Named {
  namedName :: T.Text,
  namedItem :: a
}

data Template = Template {
  tplParams :: [Named Param],
  tplApi    :: Named RApi,
  tplStage  :: Named RStage,
  tplRole   :: Named RRole,
  tplFuns   :: [LambdaRGroup]
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

newtype RRole = RRole { 
  roleName :: T.Text
}

-- | Lambda Resource Group
-- | Includes all resources for enabling a given Lambda within the API
data LambdaRGroup = LambdaRGroup {
  lamFun   :: Named RFun,   -- Function
  lamInt   :: Named RInt,   -- Integration
  lamRoute :: Named RRoute, -- Route
  lamPerm  :: Named RPerm   --
}

-- | Lambda Function Resource
data RFun = RFun {
  funName :: T.Text,
  funApi  :: Ref,
  funRole :: Ref
}

-- | API Integration
data RInt = RInt {
  intFun :: Ref,  -- Lambda Function Logical ID
  intApi :: Ref   -- API Gateway Logical ID
}

data RRoute = RRoute {
  routeFunName :: T.Text,  -- Function Name
  routeApi :: Ref,    -- API Gateway Logical ID
  routeInt :: Ref     -- API Integration Logical ID
}

-- | Lambda Permission
data RPerm = RPerm {
  permFunName :: T.Text,
  permApi   :: Ref,
  permStage :: Ref,
  permFun   :: Ref
}

newtype Ref = Ref { refId :: T.Text }

newtype Sub = Sub T.Text

newtype GetArn = GetArn T.Text

instance ToJSON Template where 
  toJSON (Template params api stage role funs) = object 
    [ "Parameters" .= object (map namedKV params)
    , "Resources"  .= object (
        [ namedKV api
        , namedKV stage
        , namedKV role
        ] ++ concatMap lambdaRGroupKVs funs
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

instance ToJSON RRole where 
  toJSON (RRole name) = object 
    [ "Type" .= fromText "AWS::IAM::Role" 
    , "Properties" .= object 
        [ "AssumeRolePolicyDocument" .= object 
            [ "Version" .= fromText "2012-10-17"
            , "Statement" .= [ object 
                [ "Effect" .= fromText "Allow"
                , "Principal" .= object 
                    [ "Service" .= fromText "lambda.amazonaws.com" ]
                , "Action" .= fromText "sts:AssumeRole"
                ]]
            ]
        , "MaxSessionDuration" .= (3600 :: Int)
        , "Path" .= fromText "/service-role/"
        , "RoleName" .= fromText name
        ]
    ]

instance ToJSON RFun where 
  toJSON (RFun name api role) = object 
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
        , "Role" .= GetArn (refId role)
        , "Runtime" .= fromText "nodejs16.x"
        ]
    ]

instance ToJSON RInt where 
  toJSON (RInt fun api) = object 
    [ "Type" .= fromText "AWS::ApiGatewayV2::Integration"
    , "Properties" .= object 
        [ "ApiId" .= api
        , "IntegrationMethod" .= fromText "POST"
        , "IntegrationType" .= fromText "AWS_PROXY"
        , "IntegrationUri" .= GetArn (refId fun)
        , "PayloadFormatVersion" .= fromText "2.0"
        ]
    ]
  
instance ToJSON RRoute where 
  toJSON (RRoute fname api int) = object 
    [ "Type" .= fromText "AWS::ApiGatewayV2::Route"
    , "Properties" .= object 
        [ "ApiId"    .= api
        , "RouteKey" .= fromText ("POST /" <> fname)
        , "Target"   .= targetFromIntRef int
        ]
    ]

instance ToJSON RPerm where 
  toJSON (RPerm fname api stage fun) = object 
    [ "Type" .= fromText "AWS::Lambda::Permission"
    , "Properties" .= object 
        [ "Action" .= fromText "lambda:InvokeFunction"
        , "FunctionName" .= GetArn (refId fun)
        , "Principal" .= fromText "apigateway.amazonaws.com"
        , "SourceArn" .= endpointSubFromRefs api stage fname
        ]
    ]

instance ToJSON Ref where 
  toJSON (Ref ref_id) = object [ "Ref" .= fromText ref_id ]

instance ToJSON Sub where 
  toJSON (Sub ref_id) = object [ "Fn::Sub" .= fromText ref_id ]

instance ToJSON GetArn where 
  toJSON (GetArn ref_id) = object [ "Fn::GetAtt" .= fromText (ref_id <> ".Arn") ]

templateFromScript :: S.Script -> Template
templateFromScript script = 
  let name  = S.scriptName script
      api   = apiFromScriptName name
      stage = stageFromApi api
      role  = roleFromScriptName name
      funs  = map (jsFunToLambda (namedRef api) 
                                 (namedRef stage) 
                                 (namedRef role)) 
                  (S.scriptFuns script)
   in Template test_params api stage role funs

jsFunToLambda :: Ref -> Ref -> Ref -> S.Fun -> LambdaRGroup
jsFunToLambda api_ref stage_ref role_ref fun = 
  let fun_name :: T.Text
      fun_name = S.funName fun

      fun_res :: Named RFun
      fun_res = Named (capitalizeFirst fun_name <> "Lambda") $ 
        RFun fun_name api_ref role_ref

      int_res :: Named RInt
      int_res = Named (capitalizeFirst fun_name <> "Integration") $ 
        RInt (namedRef fun_res) api_ref

      route_res :: Named RRoute
      route_res = Named (capitalizeFirst fun_name <> "Route") $ 
        RRoute fun_name api_ref (namedRef int_res)

      perm_res :: Named RPerm
      perm_res = Named (capitalizeFirst fun_name <> "Permission") $
        RPerm fun_name api_ref stage_ref (namedRef fun_res)
   in LambdaRGroup fun_res int_res route_res perm_res

-- Lambda Group

lambdaRGroupKVs :: LambdaRGroup -> [(Key, Value)]
lambdaRGroupKVs (LambdaRGroup fun int route perm) = 
  [namedKV fun, namedKV int, namedKV route, namedKV perm]

-- API

apiFromScriptName :: T.Text -> Named RApi
apiFromScriptName name = Named (capitalizeFirst name <> "Api") (RApi (name <> "-api")) 

stageFromApi :: Named RApi -> Named RStage
stageFromApi api = 
  let api_name = namedName api
   in Named (api_name <> "Stage") $ RStage (Ref api_name)

roleFromScriptName :: T.Text -> Named RRole
roleFromScriptName name = Named (capitalizeFirst name <> "Role") (RRole (name <> "-role"))
  
-- | name here is the resource name/logical id
hostFromApiRef :: Ref -> Sub
hostFromApiRef ref = 
  Sub $ "${" <> refId ref <> "}.execute-api.${AWS::Region}.amazonaws.com"

targetFromIntRef :: Ref -> Sub
targetFromIntRef ref = 
  Sub $ "integrations/${" <> refId ref <> "}"

endpointSubFromRefs :: Ref -> Ref -> T.Text -> Sub
endpointSubFromRefs api stage fun_name = 
  Sub $ T.concat 
    [ "arn:aws:execute-api:${AWS::Region}:${AWS::AccountId}:${"
    , refId api
    , "}/${" 
    , refId stage
    , "}/POST/"
    , fun_name
    ]

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
  [ Named (refId test_bucket_ref) $ Param "capitalizewords-bucket" ]

test_bucket_ref :: Ref
test_bucket_ref = Ref "CapitalizeWordsBucket"

test_role_ref :: Ref
test_role_ref = Ref "CapitalizeWordsRole"
 