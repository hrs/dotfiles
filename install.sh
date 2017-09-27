#!/bin/sh

stow bash
stow emacs
stow email
stow git
stow lisp
stow ruby
stow x-windows

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile
