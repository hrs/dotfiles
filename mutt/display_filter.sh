#!/usr/bin/env bash

function recode_html_entities() {
  sed s/\&\#39\;/\'/g |
    sed s/\&gt\;/\>/g |
    sed s/\&lt\;/\</g |
    sed s/\&copy\;/\(c\)/g
}

tmp_file="/tmp/mutt.txt"
cat - > "$tmp_file"

tmp_date=$(formail -x Date < "$tmp_file")
local_date=$(date -d "$tmp_date" +"%a, %d %b %Y %H:%M:%S")
message=$(formail -f -I "Date: $local_date" < "$tmp_file")

if echo "${message}" | fgrep --silent "From: Trello"; then
  echo "${message}" | trelloparse | recode_html_entities | fold -s
else
  echo "${message}" | recode_html_entities | fold -s
fi
