#!/usr/bin/env bash

set -euo pipefail
ROOT=$(dirname "$0")/..

echo "Creating distributable artifact..."

cd "$ROOT" || exit 1

rm -rf artifacts/
mkdir -p artifacts/web/{js,css,icons,screenshots}
cp _build/install/default/bin/elfeed-offline artifacts/
cp web/sw.js artifacts/web/
cp web/manifest.webmanifest artifacts/web/
cp web/js/app.js artifacts/web/js/
cp web/css/*.css artifacts/web/css/
cp web/icons/* artifacts/web/icons/
cp web/screenshots/* artifacts/web/screenshots/

tar -czf elfeed-offline.tar.gz -C artifacts elfeed-offline web/
