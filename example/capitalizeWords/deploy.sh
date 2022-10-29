#!/usr/bin/env bash 

ROLE_ARN="arn:aws:iam::565652071321:role/service-role/hello-aws-role-aoys6qk7"

package () {
  FUN_NAME="$1"

  ZIP_FILE="$FUN_NAME-aws.zip"

  echo "Creating ZIP package: $ZIP_FILE"
  zip "$ZIP_FILE" "$FUN_NAME.js"
}

create () {
  FUN_NAME="$1"

  echo "Creating: $FUN_NAME"

  ZIP_FILE="$FUN_NAME-aws.zip"

  aws lambda create-function \
    --function-name "$FUN_NAME" \
    --role "$ROLE_ARN" \
    --runtime nodejs16.x \
    --package-type Zip \
    --zip-file "fileb://$ZIP_FILE" \
    --handler "$FUN_NAME.handler"
}

update () {
  FUN_NAME="$1"

  echo "Updating: $FUN_NAME"

  ZIP_FILE="$FUN_NAME-aws.zip"

  aws lambda update-function-code \
    --function-name "$FUN_NAME" \
    --zip-file "fileb://$ZIP_FILE"
}

deploy () {
  FUN_NAME="$1"

  echo "Deploying: $FUN_NAME"

  package $FUN_NAME

  if aws lambda get-function --function-name "$FUN_NAME" > /dev/null; then 
    update $FUN_NAME
  else 
    create $FUN_NAME
  fi
}

FUN_NAMES="capitalizeWord
capitalizeWords"

for f in $FUN_NAMES; do 
  deploy "$f"
done