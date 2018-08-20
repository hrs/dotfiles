#!/bin/sh

# Install all the necessary Debian packages, especially `stow`.
./install_debian_packages.sh

stow bash
stow emacs
stow email
stow firefox
stow git
stow lisp
stow rss
stow ruby
stow x-windows

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile

# Many of the tools in this repo are written in Ruby, and some depend on
# external libraries. This installs those.
./install_ruby_gems.sh
