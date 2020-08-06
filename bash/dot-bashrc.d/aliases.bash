# -*- mode: sh -*-

alias dc="docker-compose"
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
alias myip="ip address | grep inet.*wlan0 | cut -d' ' -f6 | sed \"s/\/24//g\""
alias pbcopy="xsel --clipboard --input"
alias pbpaste="xsel --clipboard --output"
alias speedtest='echo "scale=2; `curl  --progress-bar -w "%{speed_download}" http://speedtest.wdc01.softlayer.com/downloads/test10.zip -o /dev/null` / 131072" | bc | xargs -I {} echo {} mbps'
alias tree="tree -C" # add colors
alias ut="tar xavf"

### Ruby/Rails-specific
alias be="bundle exec"
alias migrate="be rake db:migrate db:test:prepare"

### Package management
alias agi="sudo apt install"
alias agr="sudo apt remove"
alias acs="apt search"
alias ali="apt-mark showmanual"

alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias serve="python -m SimpleHTTPServer"

 complete -A file op
