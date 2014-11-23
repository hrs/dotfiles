#!/bin/bash

source $HOME/.bin/git-completion.sh
source $HOME/.bin/npm-completion.sh

### path

pathmunge () {
  if ! echo $PATH | egrep -q "(^|:)$1($|:)" ; then
    if [ "$2" = "after" ] ; then
      PATH=$PATH:$1
    else
      PATH=$1:$PATH
    fi
  fi
}

if [[ $(uname) == Darwin ]]; then
  pathmunge /usr/local/texlive/2013basic/bin/universal-darwin
fi

pathmunge /usr/local/sbin
pathmunge /usr/local/bin
pathmunge /usr/local/heroku/bin
pathmunge /sbin after
pathmunge $HOME/.bin after
pathmunge $HOME/.cask/bin after

### variables

export ALTERNATE_EDITOR=""
export PROMPT_DIRTRIM=3
export DISPLAY=:0.0
if [[ $(uname) == Darwin ]]; then
  export EDITOR="emacsclient --tty"
else
  export EDITOR="emacsclient -nw -c -a ''"
fi
export GPG_TTY=$(tty)
export LANG="en_US"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export HISTFILESIZE=20000

if [ -f "${HOME}/.gpg-agent-info" ]; then
  . "${HOME}/.gpg-agent-info"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
fi

source $HOME/.bashrc.aliases
source $HOME/.bashrc.prompt
source $HOME/.bashrc.utils

source "/usr/local/share/chruby/chruby.sh"
chruby 2.1.4

### local config settings, if any

if [ -e $HOME/.bashrc.local ]; then
  source $HOME/.bashrc.local
fi
