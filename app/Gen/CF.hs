{-# LANGUAGE OverloadedStrings #-}

module Gen.CF where 

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Key ( fromText )
import Data.Char

import qualified JS.Syntax as S

data Named a = Named {
  namedName :: T.Text,
  namedItem :: a
}

data Template = Template {
  tplBucket :: Named PBucket,
  tplApi    :: Named RApi,
  tplStage  :: Named RStage,
  tplRole   :: Named RRole,
  tplLayer  :: Named RLayer,
  tplFuns   :: [LambdaRGroup]
}

newtype PBucket = PBucket {
  bucketName :: T.Text
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

data RLayer = RLayer {
  layerName :: T.Text,
  layerBucket :: Ref
}

-- | Lambda Resource Group
-- | Includes all resources for enabling a given Lambda within the API
data LambdaRGroup = LambdaRGroup {
  lamFun   :: Named RFun,   -- Function
  lamInt   :: Named RInt,   -- Integration
  lamRoute :: Named RRoute, -- Route
  lamPerm  :: Named RPerm  -- Permission
}

-- | Lambda Function Resource
data RFun = RFun {
  funName   :: T.Text,
  funBucket :: Ref,
  funApi    :: Ref,
  funRole   :: Ref,
  funLayer  :: Ref
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
  toJSON (Template bucket api stage role layer funs) = object 
    [ "Parameters" .= object [ namedKV bucket ]
    , "Resources"  .= object (
        [ namedKV api
        , namedKV stage
        , namedKV role
        , namedKV layer
        ] ++ concatMap lambdaRGroupKVs funs
      )
    ]

instance ToJSON PBucket where 
  toJSON (PBucket name) = object 
    [ "Type" .= fromText "String"
    , "Default" .= name 
    ]

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
        , "ManagedPolicyArns" .= 
            [ fromText "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole" ]
        ]
    ]

instance ToJSON RLayer where 
  toJSON (RLayer name bucket) = object 
    [ "Type" .= fromText "AWS::Lambda::LayerVersion" 
    , "Properties" .= object 
        [ "CompatibleArchitectures" .= [ fromText "x86_64" ]
        , "CompatibleRuntimes"      .= [ fromText "nodejs22.x" ]
        , "Content" .= object 
            [ "S3Bucket" .= bucket
            , "S3Key"    .= fromText (name <> ".zip")
            ]
        , "LayerName" .= name
        ]
    ]

instance ToJSON RFun where 
  toJSON (RFun name bucket api role layer) = object 
    [ "Type" .= fromText "AWS::Lambda::Function" 
    , "Properties" .= object 
        [ "Code" .= object 
            [ "S3Bucket" .= bucket
            , "S3Key"    .= fromText (name <> "-code.zip")
            ]
        , "Environment" .= object 
            [ "Variables" .= object 
                [ "AWS_GATEWAY_HOST" .= hostFromApiRef api
                ]
            ]
        , "FunctionName" .= name
        , "Handler" .= fromText (name <> ".handler")
        , "Layers" .= [ layer ]
        , "Role" .= GetArn (refId role)
        , "Runtime" .= fromText "nodejs22.x"
        , "Timeout" .= (60 :: Int)
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
  let name   = S.scriptName script
      bucket = bucketFromScriptName name
      api    = apiFromScriptName name
      stage  = stageFromApi api
      role   = roleFromScriptName name
      layer  = layerFromScriptName (namedRef bucket) name
      funs   = map (jsFunToLambda (namedRef bucket) 
                                  (namedRef api) 
                                  (namedRef stage) 
                                  (namedRef role)
                                  (namedRef layer)) 
                   (S.scriptFuns script)
   in Template bucket api stage role layer funs

jsFunToLambda :: Ref -> Ref -> Ref -> Ref -> Ref -> S.Fun -> LambdaRGroup
jsFunToLambda bucket_ref api_ref stage_ref role_ref layer_ref fun = 
  let fun_name :: T.Text
      fun_name = S.funName fun

      fun_res :: Named RFun
      fun_res = Named (capitalizeFirst fun_name <> "Lambda") $ 
        RFun fun_name bucket_ref api_ref role_ref layer_ref

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

-- Resources

bucketFromScriptName :: T.Text -> Named PBucket
bucketFromScriptName name = 
  Named (capitalizeFirst name <> "Bucket") 
        (PBucket $ bucketNameFromScriptName name)

bucketNameFromScriptName :: T.Text -> T.Text
bucketNameFromScriptName name = T.toLower name <> "-bucket"

apiFromScriptName :: T.Text -> Named RApi
apiFromScriptName name = Named (capitalizeFirst name <> "Api") (RApi (name <> "-api")) 

stageFromApi :: Named RApi -> Named RStage
stageFromApi api = 
  let api_name = namedName api
   in Named (api_name <> "Stage") $ RStage (Ref api_name)

roleFromScriptName :: T.Text -> Named RRole
roleFromScriptName name = Named (capitalizeFirst name <> "Role") (RRole (name <> "-role"))

layerFromScriptName :: Ref -> T.Text -> Named RLayer 
layerFromScriptName bucket_ref name = 
  Named (capitalizeFirst name <> "Layer") (RLayer (name <> "-layer") bucket_ref)
  
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
