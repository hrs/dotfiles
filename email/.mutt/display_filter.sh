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
if [[ $(uname) == Darwin ]]; then
  local_date=$(gdate -d "$tmp_date" +"%a, %d %b %Y %H:%M:%S")
else
  local_date=$(date -d "$tmp_date" +"%a, %d %b %Y %H:%M:%S")
fi
message=$(formail -f -I "Date: $local_date" < "$tmp_file")

echo "${message}" | ~/.bin/reformat-email
