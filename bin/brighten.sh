#!/bin/bash

current=$(cat /sys/class/backlight/intel_backlight/brightness)
target=$(($current + 100))
max=$(cat /sys/class/backlight/intel_backlight/max_brightness)

echo $current
echo $(($current + 100))
echo $max

if [ $target -lt $max ]; then
  echo $target > /sys/class/backlight/intel_backlight/brightness
fi
