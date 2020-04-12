#!/bin/sh

# Install all the necessary Debian packages, especially `stow`.
./install_debian_packages.sh
./configure_services.sh

stow bash
stow emacs
stow email
stow firefox
stow git
stow lisp
stow music
stow ruby
stow x-windows

# Link ~/.bash_profile -> ~/.bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile

# Link ~/.config/zathura/zathurarc -> ~/.zathurarc
rm -f ~/.config/zathura/zathurarc
mkdir -p ~/.config/zathura
ln -s ~/.zathurarc ~/.config/zathura/zathurarc

# Link ~/.config/mpd/mpd.conf -> ~/.mpd.conf
rm -f ~/.config/mpd/mpd.conf
mkdir -p ~/.config/mpd
ln -s ~/.mpd.conf ~/.config/mpd/mpd.conf

# Link ~/.config/feh -> ~/feh
rm -rf ~/.config/feh
ln -s ~/.feh ~/.config/feh

# Many of the tools in this repo are written in Ruby, and some depend on
# external libraries. This installs those.
./install_ruby_gems.sh

# Similarly, we use a couple of Python packages.
./install_python_packages.sh
