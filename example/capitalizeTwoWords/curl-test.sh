#!/usr/bin/env bash

if [ -z "$1" ]; then
  echo "usage: curl-test.sh [api-gateway-id]"
fi

curl -X POST -d '"hello there"' -H 'Content-Type: application/json' "https://${1}.execute-api.us-east-2.amazonaws.com/capitalizeTwoWords"
