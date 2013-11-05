#!/bin/bash

source $HOME/.bin/git-completion.sh
source $HOME/.bin/npm-completion.sh

### path

if [[ $(uname) == Darwin ]]; then
    npm_home=/usr/local/share/npm/bin
    emacs_path=/Applications/Emacs.app/Contents/MacOS/bin
    M2_HOME=$HOME/Documents/tools/maven
    export PATH=$npm_home:$emacs_path:$PATH
fi

conf_dot_home_path=$HOME/.bin
tex_path=/usr/local/texlive/2011/bin/x86_64-darwin
rvm_bin_path=$HOME/.rvm/bin
heroku_toolbelt_path=/usr/local/heroku/bin

export PATH=$heroku_toolbelt_path:/usr/local/bin:/usr/local/sbin:$PATH:/sbin:$tex_path:$conf_dot_home_path:$rvm_bin_path
export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules

if [[ -n "$M2_HOME" ]]; then
    export PATH=$M2_HOME/bin:$PATH
fi

### variables

export DISPLAY=:0.0
export EDITOR="emacsclient -nw -c -a ''"
export LANG="en_US"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export HISTFILESIZE=20000

source $HOME/.bashrc.aliases
source $HOME/.bashrc.prompt
source $HOME/.bashrc.utils

[[ -s $HOME/.nvm/nvm.sh ]] && . $HOME/.nvm/nvm.sh # load nvm
source $HOME/.rvm/scripts/rvm # load rvm

### local config settings, if any

if [ -e $HOME/.bashrc.local ]; then
    source $HOME/.bashrc.local
fi
