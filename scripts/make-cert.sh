#!/usr/bin/env sh

set -euo pipefail

HERE=$(dirname "$0")
# Get the IPv4 address for the identified Wi-Fi interface
. "${HERE}/get-ip.sh"

echo "Generating new SSL certificates for IP address: $IP_ADDRESS"

mkdir -p ssl/
mkcert -key-file ssl/server.key -cert-file ssl/server.pem "$IP_ADDRESS" 127.0.0.1 localhost "$(hostname).local"

CERT_PATH=$(mkcert -CAROOT)/rootCA.pem
cp "$CERT_PATH" "ssl/rootCA.crt"

echo "New SSL certificates have been generated ✅"
echo ""
echo "NOTE: 📋 You need to add the '${CERT_PATH}' file to your trusted certificates."
echo "It has also been copied to ssl/rootCA.crt for your convenience."
echo "See https://github.com/FiloSottile/mkcert?tab=readme-ov-file#mobile-devices"
