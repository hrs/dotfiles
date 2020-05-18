#!/bin/sh

# Some of my tools use NPM libraries. This script is intended to be executed
# when these dotfiles are installed.
#
# To set up Node on Debian, first:
#
# $ curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | sudo apt-key add -
#
# And add the following lines to /etc/apt/sources.list:
#
#     deb https://deb.nodesource.com/node_14.x buster main
#     deb-src https://deb.nodesource.com/node_14.x buster main
#
# And finally, update npm to the latest version:
#
# $ sudo npm install npm --global

sudo npm install @postlight/mercury-parser --global
