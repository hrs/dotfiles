#!/bin/sh

# Create necessary directories
mkdir -p ~/.ssh
mkdir -p ~/.newsbeuter

# Link files from dotfiles
ln -s ~/.dotfiles/authinfo.gpg ~/.authinfo.gpg
ln -s ~/.dotfiles/bashrc ~/.bashrc
ln -s ~/.dotfiles/bashrc.d ~/.bashrc.d
ln -s ~/.dotfiles/bin ~/.bin
ln -s ~/.dotfiles/emacs.d ~/.emacs.d
ln -s ~/.dotfiles/extract_urlview ~/.extract_urlview
ln -s ~/.dotfiles/git ~/.git
ln -s ~/.dotfiles/gitconfig ~/.gitconfig
ln -s ~/.dotfiles/gitignore ~/.gitignore
ln -s ~/.dotfiles/gitmessage ~/.gitmessage
ln -s ~/.dotfiles/i3 ~/.i3
ln -s ~/.dotfiles/i3status.conf ~/.i3status.conf
ln -s ~/.dotfiles/mbsyncrc ~/.mbsyncrc
ln -s ~/.dotfiles/msmtprc ~/.msmtprc
ln -s ~/.dotfiles/mutt ~/.mutt
ln -s ~/.dotfiles/newsbeuter-config ~/.newsbeuter/config
ln -s ~/.dotfiles/rspec ~/.rspec
ln -s ~/.dotfiles/rubocop.yml ~/.rubocop.yml
ln -s ~/.dotfiles/sbclrc ~/.sbclrc
ln -s ~/.dotfiles/ssh-config ~/.ssh/config
ln -s ~/.dotfiles/wallpaper ~/.wallpaper
ln -s ~/.dotfiles/xbindkeysrc ~/.xbindkeysrc

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile
