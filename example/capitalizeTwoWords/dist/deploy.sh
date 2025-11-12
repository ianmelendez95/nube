#!/usr/bin/env bash 

set -e

BUCKET="capitalizetwowords-bucket"
TEMPLATE="capitalizeTwoWords-template.json"
STACK_NAME="capitalizeTwoWords-stack"
LAYER="capitalizeTwoWords-layer"
FUN_NAMES="capitalizeTwoWords
capitalizeTwoWords1
capitalizeTwoWords2
capitalizeWord
"

# Parse command line arguments
UPDATE_MODE=false
UPDATE_LAMBDAS=false
for arg in "$@"; do
  case $arg in
    --update-cf)
      UPDATE_MODE=true
      shift
      ;;
    --update-lambdas)
      UPDATE_LAMBDAS=true
      shift
      ;;
    *)
      echo "Unknown argument: $arg"
      echo "Usage: $0 [--update-cf|--update-lambdas]"
      exit 1
      ;;
  esac
done

assert_bucket () {
  if ! aws s3api head-bucket --bucket "$BUCKET" 2>&1 > /dev/null; then 
    read -p "Bucket '$BUCKET' not available, create? [Y/n] " -n 1 -r
    echo   
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      echo "creating bucket: $BUCKET"
      aws s3 mb "s3://$BUCKET"

      echo "disabling public access to bucket: $BUCKET"
      aws s3api put-public-access-block \
        --bucket "$BUCKET" \
        --public-access-block-configuration=BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true
    else
      echo "ERROR: cannot access bucket: $BUCKET"
      exit 1
    fi
  fi
}

upload_template () {
  aws s3 cp "$TEMPLATE" "s3://$BUCKET"
}

create_cf_stack () {
  echo "creating stack: $STACK_NAME"
  aws cloudformation create-stack \
    --template-url "https://$BUCKET.s3.us-east-2.amazonaws.com/$TEMPLATE" \
    --capabilities CAPABILITY_NAMED_IAM \
    --stack-name "$STACK_NAME" \
    --disable-rollback
}

update_lambda () {
  f="$1"

  echo "updating function layer: $f"
  aws lambda update-function-configuration \
    --no-cli-pager \
    --function-name "$f" \
    --layers "$layer_arn" > /dev/null

  aws lambda wait function-updated --function-name "$f"

  echo "updating function code: $f"
  aws lambda update-function-code \
    --no-cli-pager \
    --function-name "$f" \
    --s3-bucket "$BUCKET" \
    --s3-key "$f-code.zip" > /dev/null

  aws lambda wait function-updated --function-name "$f"
}

update_lambdas () {
  echo "updating layer: $LAYER"
  layer_response=$(aws lambda publish-layer-version \
    --no-cli-pager \
    --layer-name "$LAYER" \
    --content S3Bucket="$BUCKET",S3Key="$LAYER.zip")

  layer_arn=$(echo "$layer_response" | jq -r '.LayerVersionArn')
  if [ -z "$layer_arn" ] || [ "$layer_arn" = "null" ]; then
    echo "ERROR: failed to publish layer version"
    exit 1
  fi

  for f in $FUN_NAMES; do
    update_lambda "$f" &
  done

  wait
}

update_cf_stack () {
  echo "updating stack: $STACK_NAME"

  aws cloudformation update-stack \
    --template-url "https://$BUCKET.s3.us-east-2.amazonaws.com/$TEMPLATE" \
    --capabilities CAPABILITY_NAMED_IAM \
    --stack-name "$STACK_NAME" 
}

upload_layer () {
  ZIP_FILE="$LAYER.zip"

  echo "packaging: $ZIP_FILE"
  zip -r "$ZIP_FILE" "nodejs"

  echo "uploading: $ZIP_FILE s3://$BUCKET"
  aws s3 cp "$ZIP_FILE" "s3://$BUCKET"
}

upload_fun_code () {
  FUN_NAME="$1"

  ZIP_FILE="$FUN_NAME-code.zip"

  echo "packaging: $ZIP_FILE"
  zip -j "$ZIP_FILE" "$FUN_NAME/index.mjs"

  echo "uploading: $ZIP_FILE s3://$BUCKET"
  aws s3 cp "$ZIP_FILE" "s3://$BUCKET"
}

assert_bucket
upload_template
upload_layer

for f in $FUN_NAMES; do 
  upload_fun_code "$f"
done

if [ "$UPDATE_LAMBDAS" = true ]; then
  update_lambdas
fi

if [ "$UPDATE_MODE" = true ]; then
  update_cf_stack
else
  create_cf_stack
fi

