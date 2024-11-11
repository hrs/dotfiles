alias sudo="sudo " # effectively makes alias available when running sudo
alias c="ssh cardinal"
alias ct="ssh -t cardinal \"cd ~/media/torrents; bash --login\""
alias cat batcat
alias d="dired"
alias dc="docker-compose"
alias fd="fdfind"
alias gg="git grep -n"
alias git="hub"
alias gpg="gpg2"
alias less="less -R" # display colors correctly
alias la="ls -la"
alias ll="ls -l"
alias ln="ln -v"
alias ls="ls --color -h"
alias mkdir="mkdir -p"
alias pbcopy="xsel --clipboard --input"
alias pbpaste="xsel --clipboard --output"
alias tree="tree -C" # add colors
alias ut="tar xavf"

### Package management
alias agu="sudo $HOME/.bin/agu"
alias agi="sudo apt install"
alias agr="sudo apt remove"
alias acs="apt search"
alias ali="apt-mark showmanual"

### Network services
alias ip="ip --color=auto"
alias myip="ip -4 -brief address"
alias oports="lsof -i 4 -P -n | grep LISTEN | awk '{print \$3,\$1,\$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n | column -t | uniq"
alias serve="python -m SimpleHTTPServer"
alias speedtest='echo "scale=2; `curl  --progress-bar -w "%{speed_download}" http://speedtest.wdc01.softlayer.com/downloads/test10.zip -o /dev/null` / 131072" | bc | xargs -I {} echo {} mbps'
