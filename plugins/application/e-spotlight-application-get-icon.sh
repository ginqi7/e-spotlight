#!/bin/bash
APP_PATH="$1"
OUTPUT_FILE="$2"

echo "Application: $APP_PATH"
ICON_NAME=$(defaults read "$APP_PATH/Contents/Info" CFBundleIconFile)
if [[ "$ICON_NAME" != *".icns" ]]; then
  ICON_NAME="${ICON_NAME}.icns"
fi

ICON_PATH="$APP_PATH/Contents/Resources/$ICON_NAME"
echo "ICON: $ICON_NAME"

sips -s format png "$ICON_PATH" --out "$OUTPUT_FILE" > /dev/null

echo "Save Icon to $OUTPUT_FILE"
