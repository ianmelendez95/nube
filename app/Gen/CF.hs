module Gen.CF
  ( jsFunToLambda,
    templateFromScript,
    bucketNameFromScriptName,
  )
where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Char
import Data.Text qualified as T
import Nube.Syntax qualified as S

data Named a = Named
  { namedName :: T.Text,
    _namedItem :: a
  }

data Template = Template
  { _tplBucket :: Named PBucket,
    _tplApi :: Named RApi,
    _tplStage :: Named RStage,
    _tplRole :: Named RRole,
    _tplLayer :: Named RLayer,
    _tplFuns :: [LambdaRGroup]
  }

newtype PBucket = PBucket
  { _bucketName :: T.Text
  }

newtype RApi = RApi
  { _apiName :: T.Text
  }

newtype RStage = RStage
  { _stageApi :: Ref
  }

newtype RRole = RRole
  { _roleName :: T.Text
  }

data RLayer = RLayer
  { _layerName :: T.Text,
    _layerBucket :: Ref
  }

-- | Lambda Resource Group
-- | Includes all resources for enabling a given Lambda within the API
data LambdaRGroup = LambdaRGroup
  { _lamFun :: Named RFun, -- Function
    _lamInt :: Named RInt, -- Integration
    _lamRoute :: Named RRoute, -- Route
    _lamPerm :: Named RPerm, -- Permission
    _lamReqQueue :: Named RSQSQueue, -- SQS Request Queue
    _lamResQueue :: Named RSQSQueue, -- SQS Response Queue
    _lamSQSMap :: Named RSQSMap -- SQS Event Source Mapping
  }

-- | Lambda Function Resource
data RFun = RFun
  { _fnName :: T.Text,
    _fnucket :: Ref,
    _fnole :: Ref,
    _fnayer :: Ref
  }

-- | API Integration
data RInt = RInt
  { _intFun :: Ref, -- Lambda Function Logical ID
    _intApi :: Ref -- API Gateway Logical ID
  }

data RRoute = RRoute
  { _routeFunName :: T.Text, -- Function Name
    _routeApi :: Ref, -- API Gateway Logical ID
    _routeInt :: Ref -- API Integration Logical ID
  }

-- | Lambda Permission
data RPerm = RPerm
  { _permFunName :: T.Text,
    _permApi :: Ref,
    _permStage :: Ref,
    _permFun :: Ref
  }

newtype RSQSQueue = RSQSQueue
  { _sqsQueueName :: T.Text
  }

data RSQSMap = RSQSMap
  { _sqsMapQueue :: Ref,
    _sqsMapFun :: Ref
  }

newtype Ref = Ref {refId :: T.Text}

newtype Sub = Sub T.Text

newtype GetArn = GetArn T.Text

instance ToJSON Template where
  toJSON (Template bucket api stage role layer funs) =
    object
      [ "Parameters" .= object [namedKV bucket],
        "Resources"
          .= object
            ( [ namedKV api,
                namedKV stage,
                namedKV role,
                namedKV layer,
                ("ResponseTable", responseTable)
              ]
                ++ concatMap lambdaRGroupKVs funs
            )
      ]

instance ToJSON PBucket where
  toJSON (PBucket name) =
    object
      [ "Type" .= fromText "String",
        "Default" .= name
      ]

instance ToJSON RApi where
  toJSON (RApi name) =
    object
      [ "Type" .= fromText "AWS::ApiGatewayV2::Api",
        "Properties"
          .= object
            [ "Name" .= name,
              "ProtocolType" .= fromText "HTTP"
            ]
      ]

instance ToJSON RStage where
  toJSON (RStage api) =
    object
      [ "Type" .= fromText "AWS::ApiGatewayV2::Stage",
        "Properties"
          .= object
            [ "ApiId" .= api,
              "AutoDeploy" .= True,
              "StageName" .= fromText "$default"
            ]
      ]

instance ToJSON RRole where
  toJSON (RRole name) =
    object
      [ "Type" .= fromText "AWS::IAM::Role",
        "Properties"
          .= object
            [ "AssumeRolePolicyDocument"
                .= object
                  [ "Version" .= fromText "2012-10-17",
                    "Statement"
                      .= [ object
                             [ "Effect" .= fromText "Allow",
                               "Principal"
                                 .= object
                                   ["Service" .= fromText "lambda.amazonaws.com"],
                               "Action" .= fromText "sts:AssumeRole"
                             ]
                         ]
                  ],
              "MaxSessionDuration" .= (3600 :: Int),
              "Path" .= fromText "/service-role/",
              "RoleName" .= fromText name,
              "ManagedPolicyArns"
                .= [ fromText "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole",
                     fromText "arn:aws:iam::aws:policy/service-role/AWSLambdaSQSQueueExecutionRole"
                   ],
              "Policies"
                .= [ object
                       [ "PolicyName" .= fromText "SQSAndDynamoPolicy",
                         "PolicyDocument"
                           .= object
                             [ "Version" .= fromText "2012-10-17",
                               "Statement"
                                 .= [ object
                                        [ "Effect" .= fromText "Allow",
                                          "Action"
                                            .= [fromText "sqs:SendMessage"],
                                          "Resource"
                                            .= object
                                              ["Fn::Sub" .= fromText "arn:aws:sqs:${AWS::Region}:${AWS::AccountId}:*-queue"]
                                        ],
                                      object
                                        [ "Effect" .= fromText "Allow",
                                          "Action"
                                            .= [ fromText "dynamodb:PutItem",
                                                 fromText "dynamodb:GetItem",
                                                 fromText "dynamodb:UpdateItem",
                                                 fromText "dynamodb:DeleteItem",
                                                 fromText "dynamodb:Query"
                                               ],
                                          "Resource"
                                            .= object
                                              ["Fn::GetAtt" .= fromText "ResponseTable.Arn"]
                                        ]
                                    ]
                             ]
                       ]
                   ]
            ]
      ]

{--
{
    "Action": [
        "dynamodb:PutItem",
        "dynamodb:GetItem",
        "dynamodb:UpdateItem",
        "dynamodb:DeleteItem",
        "dynamodb:Query"
    ],
    "Effect": "Allow",
    "Resource": {
        "Fn::GetAtt": "ResponseTable.Arn"
    }

--}

instance ToJSON RLayer where
  toJSON (RLayer name bucket) =
    object
      [ "Type" .= fromText "AWS::Lambda::LayerVersion",
        "Properties"
          .= object
            [ "CompatibleArchitectures" .= [fromText "x86_64"],
              "CompatibleRuntimes" .= [fromText "nodejs22.x"],
              "Content"
                .= object
                  [ "S3Bucket" .= bucket,
                    "S3Key" .= fromText (name <> ".zip")
                  ],
              "LayerName" .= name
            ]
      ]

instance ToJSON RFun where
  toJSON (RFun name bucket role layer) =
    object
      [ "Type" .= fromText "AWS::Lambda::Function",
        "Properties"
          .= object
            [ "Code"
                .= object
                  [ "S3Bucket" .= bucket,
                    "S3Key" .= fromText (name <> "-code.zip")
                  ],
              "Environment"
                .= object
                  [ "Variables"
                      .= object
                        [ "SQS_BASE_URL"
                            .= object
                              ["Fn::Sub" .= fromText "https://sqs.${AWS::Region}.amazonaws.com/${AWS::AccountId}"]
                        ]
                  ],
              "FunctionName" .= name,
              "Handler" .= fromText "index.handler",
              "Layers" .= [layer],
              "Role" .= GetArn (refId role),
              "Runtime" .= fromText "nodejs22.x",
              "Timeout" .= (20 :: Int)
            ]
      ]

instance ToJSON RInt where
  toJSON (RInt fn api) =
    object
      [ "Type" .= fromText "AWS::ApiGatewayV2::Integration",
        "Properties"
          .= object
            [ "ApiId" .= api,
              "IntegrationMethod" .= fromText "POST",
              "IntegrationType" .= fromText "AWS_PROXY",
              "IntegrationUri" .= GetArn (refId fn),
              "PayloadFormatVersion" .= fromText "2.0"
            ]
      ]

instance ToJSON RRoute where
  toJSON (RRoute fname api int) =
    object
      [ "Type" .= fromText "AWS::ApiGatewayV2::Route",
        "Properties"
          .= object
            [ "ApiId" .= api,
              "RouteKey" .= fromText ("POST /" <> fname),
              "Target" .= targetFromIntRef int
            ]
      ]

instance ToJSON RPerm where
  toJSON (RPerm fname api stage fn) =
    object
      [ "Type" .= fromText "AWS::Lambda::Permission",
        "Properties"
          .= object
            [ "Action" .= fromText "lambda:InvokeFunction",
              "FunctionName" .= GetArn (refId fn),
              "Principal" .= fromText "apigateway.amazonaws.com",
              "SourceArn" .= endpointSubFromRefs api stage fname
            ]
      ]

instance ToJSON RSQSQueue where
  toJSON (RSQSQueue name) =
    object
      [ "Type" .= fromText "AWS::SQS::Queue",
        "Properties"
          .= object
            [ "QueueName" .= name,
              "VisibilityTimeout" .= (20 :: Int), -- must match the lambda timeout (20 is the default)
              "MessageRetentionPeriod" .= (60 :: Int),
              "ReceiveMessageWaitTimeSeconds" .= (5 :: Int)
            ]
      ]

instance ToJSON RSQSMap where
  toJSON (RSQSMap queue fn) =
    object
      [ "Type" .= fromText "AWS::Lambda::EventSourceMapping",
        "Properties"
          .= object
            [ "EventSourceArn"
                .= object
                  ["Fn::GetAtt" .= fromText (refId queue <> ".Arn")],
              "FunctionName"
                .= object
                  ["Ref" .= refId fn],
              "BatchSize" .= (10 :: Int),
              "MaximumBatchingWindowInSeconds" .= (5 :: Int)
            ]
      ]

instance ToJSON Ref where
  toJSON (Ref ref_id) = object ["Ref" .= fromText ref_id]

instance ToJSON Sub where
  toJSON (Sub ref_id) = object ["Fn::Sub" .= fromText ref_id]

instance ToJSON GetArn where
  toJSON (GetArn ref_id) = object ["Fn::GetAtt" .= fromText (ref_id <> ".Arn")]

templateFromScript :: S.Script -> Template
templateFromScript script =
  let name = S.scriptName script
      bucket = bucketFromScriptName name
      api = apiFromScriptName name
      stage = stageFromApi api
      role = roleFromScriptName name
      layer = layerFromScriptName (namedRef bucket) name
      funs =
        map
          ( jsFunToLambda
              (namedRef bucket)
              (namedRef api)
              (namedRef stage)
              (namedRef role)
              (namedRef layer)
          )
          (S.scriptFns script)
   in Template bucket api stage role layer funs

jsFunToLambda :: Ref -> Ref -> Ref -> Ref -> Ref -> S.Fn -> LambdaRGroup
jsFunToLambda bucket_ref api_ref stage_ref role_ref layer_ref fn =
  LambdaRGroup
    fun_res
    int_res
    route_res
    perm_res
    req_queue_res
    res_queue_res
    sqs_map_res
  where
    fun_name :: T.Text
    fun_name = S.fnName fn

    fun_res :: Named RFun
    fun_res =
      Named (capitalizeFirst fun_name <> "Lambda") $
        RFun fun_name bucket_ref role_ref layer_ref

    int_res :: Named RInt
    int_res =
      Named (capitalizeFirst fun_name <> "Integration") $
        RInt (namedRef fun_res) api_ref

    route_res :: Named RRoute
    route_res =
      Named (capitalizeFirst fun_name <> "Route") $
        RRoute fun_name api_ref (namedRef int_res)

    perm_res :: Named RPerm
    perm_res =
      Named (capitalizeFirst fun_name <> "Permission") $
        RPerm fun_name api_ref stage_ref (namedRef fun_res)

    req_queue_res :: Named RSQSQueue
    req_queue_res =
      Named (capitalizeFirst fun_name <> "RequestQueue") $
        RSQSQueue (fun_name <> "-request-queue")

    res_queue_res :: Named RSQSQueue
    res_queue_res =
      Named (capitalizeFirst fun_name <> "ResponseQueue") $
        RSQSQueue (fun_name <> "-response-queue")

    sqs_map_res :: Named RSQSMap
    sqs_map_res =
      Named (capitalizeFirst fun_name <> "SQSMapping") $
        RSQSMap (namedRef req_queue_res) (namedRef fun_res)

-- Lambda Group

lambdaRGroupKVs :: LambdaRGroup -> [(Key, Value)]
lambdaRGroupKVs (LambdaRGroup fn int route perm req_queue res_queue sqs_map) =
  [namedKV fn, namedKV int, namedKV route, namedKV perm, namedKV req_queue, namedKV res_queue, namedKV sqs_map]

-- Resources

responseTable :: Value
responseTable =
  object
    [ "Type" .= fromText "AWS::DynamoDB::Table",
      "Properties"
        .= object
          [ "TableName" .= fromText "response-table",
            "TableClass" .= fromText "STANDARD",
            "AttributeDefinitions"
              .= [ object
                     [ "AttributeName" .= fromText "request-id",
                       "AttributeType" .= fromText "S"
                     ]
                 ],
            "KeySchema"
              .= [ object
                     [ "AttributeName" .= fromText "request-id",
                       "KeyType" .= fromText "HASH"
                     ]
                 ],
            "BillingMode" .= fromText "PAY_PER_REQUEST"
          ]
    ]

bucketFromScriptName :: T.Text -> Named PBucket
bucketFromScriptName name =
  Named
    (capitalizeFirst name <> "Bucket")
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

targetFromIntRef :: Ref -> Sub
targetFromIntRef ref =
  Sub $ "integrations/${" <> refId ref <> "}"

endpointSubFromRefs :: Ref -> Ref -> T.Text -> Sub
endpointSubFromRefs api stage fun_name =
  Sub $
    T.concat
      [ "arn:aws:execute-api:${AWS::Region}:${AWS::AccountId}:${",
        refId api,
        "}/${",
        refId stage,
        "}/POST/",
        fun_name
      ]

-- Named

namedKV :: (ToJSON a) => Named a -> (Key, Value)
namedKV (Named name value) = (fromText name, toJSON value)

namedRef :: Named a -> Ref
namedRef = Ref . namedName

-- Text

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst txt =
  case T.uncons txt of
    Nothing -> txt
    Just (c, cs) -> toUpper c `T.cons` cs
