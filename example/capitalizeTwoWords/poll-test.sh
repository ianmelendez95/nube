#!/usr/bin/env bash

if [ -z "$2" ]; then
  echo "usage: curl-test.sh api-gateway-id frame-id"
fi

curl -X POST -H 'Content-Type: application/json' "https://${1}.execute-api.us-east-2.amazonaws.com/capitalizeTwoWords?check=${2}"
