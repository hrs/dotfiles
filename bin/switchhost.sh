#!/bin/bash

environment=$1

if [ -e "/etc/hosts.$environment" ]; then
    sudo rm -f /etc/hosts
    sudo ln -s "/etc/hosts.$environment" /etc/hosts
    sudo dscacheutil -flushcache
else
    echo "hosts file /etc/hosts.$environment doesn't exist; no changes made." 1>&2
    exit 1
fi
