#!/usr/bin/env bash 

FUN_NAME="capitalizeWord"
ROLE_ARN="arn:aws:iam::565652071321:role/service-role/hello-aws-role-aoys6qk7"

ZIP_FILE="$FUN_NAME-aws.zip"

zip "$ZIP_FILE" index.js
aws lambda create-function \
  --function-name "$FUN_NAME" \
  --role "$ROLE_ARN" \
  --runtime nodejs16.x \
  --package-type Zip \
  --zip-file "fileb://$ZIP_FILE" \
  --handler index.handler