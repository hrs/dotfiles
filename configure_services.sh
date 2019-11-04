#!/bin/sh

# Stop the mpd daemon and ensure that it doesn't start again. Instead of using
# the sysmem mpd daemon, I just start a user-specific one when logging in.
sudo service stop mpd
sudo systemctl disable mpd
