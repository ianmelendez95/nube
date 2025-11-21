#!/usr/bin/env bash

if [ -z "$2" ]; then
  echo "usage: poll-test.sh api-gateway-id frame-id"
  exit 1
fi

API_ID="$1"
FRAME_ID="$2"
URL="https://${API_ID}.execute-api.us-east-2.amazonaws.com/capitalizeTwoWords?check=${FRAME_ID}"

echo "Polling for result..."

while true; do
  RESPONSE=$(curl -s -w "\n%{http_code}" -X POST -H 'Content-Type: application/json' "$URL")
  HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
  BODY=$(echo "$RESPONSE" | sed '$d')
  
  if [ "$HTTP_CODE" = "200" ]; then
    echo "Result ready!"
    echo "$BODY"
    break
  elif [ "$HTTP_CODE" = "202" ]; then
    echo "Still processing... (202)"
    sleep 1
  else
    echo "Unexpected status code: $HTTP_CODE"
    echo "$BODY"
    break
  fi
done
