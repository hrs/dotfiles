#!/bin/bash

brightness_file="/sys/class/backlight/intel_backlight/brightness"

if [ ! -w "$brightness_file" ]; then
  gksudo chmod a+w "$brightness_file"
fi

current=$(cat "$brightness_file")
target=$(($current + 100))
max=$(cat /sys/class/backlight/intel_backlight/max_brightness)

echo $current
echo $(($current + 100))
echo $max

if [ $target -lt $max ]; then
  echo $target > "$brightness_file"
fi
