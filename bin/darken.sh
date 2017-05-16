#!/bin/bash

current=$(cat /sys/class/backlight/intel_backlight/brightness)
target=$(($current - 100))
min=50

echo $current
echo $(($current - 100))
echo $min

if [ $target -gt $min ]; then
  echo $target > /sys/class/backlight/intel_backlight/brightness
fi
