source $HOME/.bin/git-completion.sh
source $HOME/.bin/npm-completion.sh

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient --tty"
export EMAIL="hello@harryrschwartz.com"
export GPG_TTY=$(tty)
export GPODDER_HOME="/home/hrs/downloads/podcasts"
export HISTFILESIZE=20000
export LANG="en_US"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export NAME="Harry R. Schwartz"
export PROMPT_DIRTRIM=3

if [[ $(uname) == Darwin ]]; then
  # otherwise, I'm starting the agent with i3
  if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
  fi
fi

if [[ $(uname) == Darwin ]]; then
  source /usr/local/etc/bash_completion.d/password-store
else
  source /etc/bash_completion.d/password-store
fi

source "/usr/local/share/chruby/chruby.sh"
chruby 2.3.0

### local config settings, if any

if [ -e $HOME/.bashrc.local ]; then
  source $HOME/.bashrc.local
fi
