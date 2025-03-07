#!/bin/sh

# Many of my tools (and my development environment) use Ruby libraries. This
# installs the necessary ones.
#
# This script is intended to be executed when these dotfiles are installed. It
# may also be useful to run this script whenever a new default version of Ruby
# is installed -- I always forget a few when I'm doing it manually.

gem install \
    activesupport \
    awesome_print \
    bundle \
    fastri \
    fuzz \
    org-ruby \
    os \
    rcodetools \
    reek \
    rest-client \
    ricepaper \
    rspec \
    rubocop \
    solargraph \
    sorbet
