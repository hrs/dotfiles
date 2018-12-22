source $HOME/.bin/git-completion.sh

export ALTERNATE_EDITOR=""
export BROWSER="firefox"
export EDITOR="$HOME/.bin/em"
export EMAIL="hello@harryrschwartz.com"
export GOPATH="$HOME/code/go"
export GPG_TTY=$(tty)
export HISTFILESIZE=20000
export HOMEBREW_NO_ANALYTICS=1
export LANG="en_US"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export NAME="Harry R. Schwartz"
export PROMPT_DIRTRIM=3

source /etc/bash_completion.d/password-store

source "/usr/local/share/chruby/chruby.sh"
chruby 2.5.3

### local config settings, if any

if [ -e $HOME/.bashrc.local ]; then
  source $HOME/.bashrc.local
fi
