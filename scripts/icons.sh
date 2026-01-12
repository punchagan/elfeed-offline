#!/usr/bin/env sh

cd "$(dirname "$0")"/.. || exit 1

for size in 48 72 96 128 144 152 192 256 384 512; do
    echo "Generating icon size ${size}x${size}"
    magick -background none web/icons/icon.svg -resize ${size}x${size} web/icons/icon-${size}x${size}.png
done

# Replace <rect /> before generating maskable image
sed 's/<rect x="1" y="1" width="22" height="22" rx="4" fill="#0F172A" \/>/<rect x="0" y="0" width="24" height="24" rx="0" fill="#0F172A" \/>/' web/icons/icon.svg > web/icons/icon-maskable.svg

for size in 192 512; do
  echo "Generating maskable icon size ${size}x${size}"
  magick -background "#0F172A" web/icons/icon-maskable.svg \
    -resize $((size * 80 / 100))x$((size * 80 / 100)) \
    -gravity center -extent ${size}x${size} \
    -alpha remove -alpha off \
    web/icons/icon-maskable-${size}x${size}.png
done

# Generate favicon.ico
echo "Generating favicon.ico"
magick web/icons/icon-192x192.png \
  -resize 32x32 -resize 16x16 \
  web/favicon.ico
