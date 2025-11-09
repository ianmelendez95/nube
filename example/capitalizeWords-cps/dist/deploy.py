#!/usr/bin/env python3

BUCKET = "capitalizewords-bucket"
TEMPLATE = "capitalizeWords-template.json"
STACK_NAME = "capitalizeWords-stack"
LAYER = "capitalizeWords-layer"
FUN_NAMES = ["capitalizeWords", "capitalizeWord"]

def assert_bucket():

#   if ! aws s3api head-bucket --bucket "$BUCKET" 2>&1 > /dev/null; then 
#     read -p "Bucket '$BUCKET' not available, create? [Y/n] " -n 1 -r
#     echo   
#     if [[ $REPLY =~ ^[Yy]$ ]]; then
#       echo "creating bucket: $BUCKET"
#       aws s3 mb "s3://$BUCKET"

#       echo "disabling public access to bucket: $BUCKET"
#       aws s3api put-public-access-block \
#         --bucket "$BUCKET" \
#         --public-access-block-configuration=BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true
#     else
#       echo "ERROR: cannot access bucket: $BUCKET"
#       exit 1
#     fi
#   fi