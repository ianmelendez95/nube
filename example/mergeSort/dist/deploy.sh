#!/usr/bin/env bash 

set -e

BUCKET="mergesort-bucket"
TEMPLATE="mergeSort-template.json"
STACK_NAME="mergeSort-stack"
LAYER="mergeSort-layer"
FUN_NAMES="testMergeSort
getRandomIntArray
getRandomInt
mergeSort
mergeSortPair
"

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
  echo "creating stack: $TEMPLATE"
  aws cloudformation create-stack \
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
  zip "$ZIP_FILE" "$FUN_NAME.js"

  echo "uploading: $ZIP_FILE s3://$BUCKET"
  aws s3 cp "$ZIP_FILE" "s3://$BUCKET"
}

assert_bucket
upload_template
upload_layer

for f in $FUN_NAMES; do 
  upload_fun_code "$f"
done

create_cf_stack

