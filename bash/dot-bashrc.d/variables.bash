source $HOME/.bin/git-completion.sh

export ALTERNATE_EDITOR=""
export BROWSER="firefox"
export EDITOR="$HOME/.bin/em"
export EMAIL="hello@harryrschwartz.com"
export GOPATH="$HOME/code/go"
export GPG_TTY=$(tty)
export HISTFILESIZE=20000
export LANG="en_CA.UTF-8"
export LC_ALL="en_CA.UTF-8"
export LC_CTYPE="en_CA.UTF-8"
export NAME="Harry R. Schwartz"
export PROMPT_DIRTRIM=3
export TEXMFHOME="$HOME/.texmf"
export TEXINPUTS=$TEXMFHOME:$TEXINPUTS
export Z3_EXE="/usr/bin/z3"

# Don't spy on me, folks.
export HOMEBREW_NO_ANALYTICS=1
export SAM_CLI_TELEMETRY=0

# Enable tab-completion in various other contexts, including make targets and
# man pages.
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

source "$HOME/.cargo/env"

test -r $HOME/.opam/opam-init/init.sh && . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
