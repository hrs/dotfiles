alias sudo="sudo " # effectively makes alias available when running sudo
abbr --add cat batcat
abbr --add d dired
abbr --add dc docker-compose
abbr --add fd fdfind
abbr --add gg "git grep -n"
abbr --add git gh
abbr --add gpg gpg2
abbr --add less "less -R"
abbr --add ls eza
abbr --add mkdir "mkdir -p"
abbr --add pbcopy "xsel --clipboard --input"
abbr --add pbpaste "xsel --clipboard --output"
abbr --add sc "ssh cardinal"
abbr --add tree "tree -C"

### Package management
alias agu="sudo $HOME/.bin/agu"
abbr --add agi "sudo apt install"
abbr --add agr "sudo apt remove"
abbr --add acs "apt search"
abbr --add ali "apt-mark showmanual"

### Network services
alias ip="ip --color=auto"
alias myip="ip -4 -brief address"
alias oports="lsof -i 4 -P -n | grep LISTEN | awk '{print \$3,\$1,\$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n | column -t | uniq"
alias serve="python -m SimpleHTTPServer"
alias speedtest='echo "scale=2; `curl  --progress-bar -w "%{speed_download}" http://speedtest.wdc01.softlayer.com/downloads/test10.zip -o /dev/null` / 131072" | bc | xargs -I {} echo {} mbps'
