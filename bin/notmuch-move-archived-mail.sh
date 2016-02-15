#!/bin/sh

function archive() {
  if [ $# -ne 2 ]; then
    echo "Incorrent parameters. Send INBOX and ARCHIVE"
    return 1
  fi

  for e in $(notmuch search --output=files "path:$1/**" and not tag:inbox); do
    mv $e $2
  done

  return 0
}

archive cur ~/.mail/personal/archive/cur
archive cur ~/.mail/thoughtbot/archive/cur
