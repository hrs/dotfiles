# -*- mode: sh -*-

alias gg="git grep -n"
alias git="hub"
alias gpg="gpg2"
alias less="less -R" # display colors correctly
alias sbcl="rlwrap sbcl"
alias lisp="sbcl --noinform"
alias lispi="sbcl -noinform --load"
alias la="ls -la"
alias ll="ls -l"
alias ln="ln -v"
alias ls="ls --color -h"
alias mkdir="mkdir -p"
alias myip="ifconfig wlan0 | grep 'inet ' | cut --delimiter=' ' -f12 | sed s/addr://"
alias pbcopy="xsel --clipboard --input"
alias pbpaste="xsel --clipboard --output"
alias rss="newsbeuter -q -u ~/documents/rss/urls"
alias rsync='rsync -vzahP'
alias scp="rsync"
alias tree="tree -C" # add colors
alias ut="tar xavf"

usage() {
  du -sch "$@" | sort -h
}

### Ruby/Rails-specific
alias be="bundle exec"
alias migrate="be rake db:migrate db:test:prepare"

### Package management
alias agi="sudo apt install"
alias agr="sudo apt remove"
alias acs="apt search"
alias agu="sudo apt update && sudo apt full-upgrade && sudo apt autoremove && sudo aptitude clean"
alias ali="apt-mark showmanual"

alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias serve="python -m SimpleHTTPServer"
