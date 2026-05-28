#!/usr/bin/env bash

set -xeuo pipefail

PLATFORM="${1:?platform name required}"
VERSION="${2:-dev}"

# Build the project
echo "Building elfeed-offline version ${VERSION}..."
dune build @install --release --only-packages elfeed-offline --display=short

# Create a tarball of the project
OUT_NAME="elfeed-offline-${VERSION}-${PLATFORM}"
mkdir -p "$OUT_NAME"
cp -rlf _build/install/default/bin/elfeed-offline "$OUT_NAME"
cp -rlf _build/install/default/doc/elfeed-offline/{README.org,LICENSE} "$OUT_NAME"
tar --format=posix -cf "$OUT_NAME.tar" "$OUT_NAME"
gzip -v9 "${OUT_NAME}.tar"
