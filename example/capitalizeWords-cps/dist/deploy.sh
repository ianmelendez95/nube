#!/usr/bin/env bash 

set -e

BUCKET="capitalizewords-bucket"
TEMPLATE="capitalizeWords-template.json"
STACK_NAME="capitalizeWords-stack"
LAYER="capitalizeWords-layer"
FUN_NAMES="capitalizeWords
capitalizeWord
"

# Parse command line arguments
UPDATE_STACK=false
CHECK_DIFF=false

while [[ $# -gt 0 ]]; do
  case $1 in
    --update)
      UPDATE_STACK=true
      shift
      ;;
    --diff)
      CHECK_DIFF=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--update|--diff]"
      echo "  --update    Update existing stack instead of creating new one"
      echo "  --diff      Show differences between local template and deployed stack"
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

update_cf_stack () {
  echo "updating stack: $STACK_NAME"
  aws cloudformation update-stack \
    --template-url "https://$BUCKET.s3.us-east-2.amazonaws.com/$TEMPLATE" \
    --capabilities CAPABILITY_NAMED_IAM \
    --stack-name "$STACK_NAME" 
}

check_diff () {
  echo "checking template differences for stack: $STACK_NAME"
  
  # Check if stack exists
  if ! aws cloudformation describe-stacks --stack-name "$STACK_NAME" &>/dev/null; then
    echo "❌ Stack '$STACK_NAME' does not exist - nothing to compare"
    exit 1
  fi
  
  # Create temporary files for comparison
  LOCAL_TEMPLATE=$(mktemp)
  DEPLOYED_TEMPLATE=$(mktemp)
  
  # Clean up temp files on exit
  trap "rm -f $LOCAL_TEMPLATE $DEPLOYED_TEMPLATE" EXIT
  
  # Get local template (pretty-printed)
  cat "$TEMPLATE" | jq '.' > "$LOCAL_TEMPLATE"
  
  # Get deployed template (pretty-printed)
  echo "fetching deployed template..."
  aws cloudformation get-template \
    --stack-name "$STACK_NAME" \
    --query 'TemplateBody' \
    --output json | jq '.' > "$DEPLOYED_TEMPLATE"
  
  # Compare templates
  echo ""
  echo "=== Template Diff (Local vs Deployed) ==="
  echo "Legend: - (deployed/old)  + (local/new)"
  echo ""
  
  if diff -u "$DEPLOYED_TEMPLATE" "$LOCAL_TEMPLATE"; then
    echo "✅ No differences found - templates are identical"
  else
    echo ""
    echo "💡 Use './deploy.sh --update' to apply these changes"
  fi
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

if [ "$CHECK_DIFF" = true ]; then
  # For diff checking, we don't need to upload anything
  check_diff
else
  # Normal deployment flow
  assert_bucket
  upload_template
  upload_layer

  for f in $FUN_NAMES; do 
    upload_fun_code "$f"
  done

  if [ "$UPDATE_STACK" = true ]; then
    update_cf_stack
  else
    create_cf_stack
  fi
fi

