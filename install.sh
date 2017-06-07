#!/bin/sh

stow bash
stow emacs
stow email
stow git
stow i3
stow lisp
stow ruby

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile
