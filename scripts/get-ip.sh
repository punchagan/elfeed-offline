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
    echo "Found IP address: $IP_ADDRESS on interface $WIFI_INTERFACE"
fi
export IP_ADDRESS
