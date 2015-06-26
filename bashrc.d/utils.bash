# -*- mode: sh -*-

function countpage() {
  pdf2dsc "$1" /dev/stdout | grep "Pages" | sed s/[^0-9]//g
}

function viewpages() {
  for file in "$@"; do
    filepagecount=$(countpage "$file")
    printf "%4d %s\n" $filepagecount $file
  done
}
