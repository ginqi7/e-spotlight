#!/bin/bash
APP_PATH="$1"
OUTPUT_FILE="$2"

echo "Application: $APP_PATH"
ICON_NAME=$(defaults read "$APP_PATH/Contents/Info" CFBundleIconFile) || ICON_NAME="icon"
if [[ "$ICON_NAME" != *".icns" ]]; then
  ICON_NAME="${ICON_NAME}.icns"
fi

ICON_PATH="$APP_PATH/Contents/Resources/$ICON_NAME"
echo "ICON: $ICON_NAME"
if [ -f "ICON_PATH" ]; then
    sips -s format png "$ICON_PATH" --out "$OUTPUT_FILE" > /dev/null
else
    echo "No icon in $APP_PATH"
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    ICON_PATH="$SCRIPT_DIR/icon.png"
    echo "$ICON_PATH"
    cp "$ICON_PATH" "$OUTPUT_FILE"
fi
