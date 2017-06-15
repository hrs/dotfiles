#!/bin/bash

# Launch rofi with Inconsolata and an adaptation of the solarized-dark theme.

PATH=~/bin:~/.bin:$PATH rofi \
    -show run \
    -font "Inconsolata 24" \
    -color-enabled: true \
    -color-window "#002b37,#002b37,#003642" \
    -color-normal "#002b37,#819396,#003643,#008ed4,#ffffff" \
    -color-active "#002b37,#008ed4,#003643,#008ed4,#66c6ff" \
    -color-urgent "#002b37,#da4281,#003643,#008ed4,#890661"
