#!/usr/bin/env bash 

set -e

BUCKET="capitalizewords-bucket"
TEMPLATE="capitalizeWords-template.json"
STACK_NAME="capitalizeWords-stack"
FUN_NAMES="capitalizeWords
capitalizeWord
"

assert_bucket () {
  if ! aws s3api head-bucket --bucket "$BUCKET" 2>&1 > /dev/null; then 
    echo "ERROR: cannot access bucket: $BUCKET"
    exit 1
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

for f in $FUN_NAMES; do 
  upload_fun_code "$f"
done

create_cf_stack

