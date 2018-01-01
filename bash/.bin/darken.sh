#!/bin/bash

brightness_file="/sys/class/backlight/intel_backlight/brightness"

if [ ! -w "$brightness_file" ]; then
  gksudo chmod a+w "$brightness_file"
fi

current=$(cat "$brightness_file")
target=$(($current - 100))
min=50

echo $current
echo $(($current - 100))
echo $min

if [ $target -gt $min ]; then
  echo $target > "$brightness_file"
fi
