#!/usr/bin/env sh

set -euo pipefail

HERE=$(dirname "$0")
. "${HERE}/get-ip.sh"

avahi-publish -a -R $(hostname).local "${IP_ADDRESS}"
