source $HOME/.bin/git-completion.sh

export ALTERNATE_EDITOR=""
export BROWSER="firefox"
export EDITOR="$HOME/.bin/em"
export EMAIL="hello@harryrschwartz.com"
export GOPATH="$HOME/code/go"
export GPG_TTY=$(tty)
export HISTFILESIZE=20000
export LANG="en_US"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export NAME="Harry R. Schwartz"
export PROMPT_DIRTRIM=3
export TEXINPUTS=/home/hrs/.texmf:$TEXINPUTS

# Don't spy on me, folks.
export HOMEBREW_NO_ANALYTICS=1
export SAM_CLI_TELEMETRY=0

# Enable tab-completion in pass.
source /etc/bash_completion.d/password-store

# Enable tab-completion in various other contexts, including make targets and
# man pages.
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
chruby 2.7.1

eval $(opam env)

### local config settings, if any

if [ -e $HOME/.bashrc.local ]; then
  source $HOME/.bashrc.local
fi
