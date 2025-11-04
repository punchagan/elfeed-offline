#!/usr/bin/env sh

set -euo pipefail

WIFI_INTERFACE=$(ip link show | grep -E 'wl|wlan' | awk -F': ' '{print $2}' | head -n 1)

if [ -z "$WIFI_INTERFACE" ]; then
    echo "No Wi-Fi interface found."
    exit 1
fi

# Get the IPv4 address for the identified Wi-Fi interface
IP_ADDRESS=$(ip addr show dev "$WIFI_INTERFACE" | grep "inet " | awk '{print $2}' | cut -d/ -f1)

if [ -z "$IP_ADDRESS" ]; then
    echo "No IP address found for interface $WIFI_INTERFACE."
else
    echo "Wi-Fi IP Address ($WIFI_INTERFACE): $IP_ADDRESS"
fi

mkdir -p ssl/
mkcert -key-file ssl/server.key -cert-file ssl/server.pem "$IP_ADDRESS" 127.0.0.1 localhost

echo "New SSL certificates have been generated ✅"

CERT_PATH=$(mkcert -CAROOT)/rootCA.pem
echo ""
echo "NOTE: 📋 You need to add the '${CERT_PATH}' file to your trusted certificates."
echo "See https://github.com/FiloSottile/mkcert?tab=readme-ov-file#mobile-devices"
