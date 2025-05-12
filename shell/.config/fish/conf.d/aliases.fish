alias sudo="sudo " # effectively makes alias available when running sudo

if type -q bat
  abbr --add cat bat
else if type -q batcat
  abbr --add cat batcat
end

abbr --add d dired
abbr --add dc docker-compose

if type -q fdfind
  abbr --add fd fdfind
end

if type -q gpg2
  abbr --add gpg gpg2
end

abbr --add less "less -R"
abbr --add ls eza
abbr --add mkdir "mkdir -p"
abbr --add sc "ssh cardinal"
abbr --add tree "tree -C"

if test not (type -q pbcopy)
    abbr --add pbcopy "xsel --clipboard --input"
    abbr --add pbpaste "xsel --clipboard --output"
end

### Package management
switch (uname)
    case Linux
        alias agu="sudo $HOME/.bin/agu"
        abbr --add agi "sudo apt install"
        abbr --add agr "sudo apt remove"
        abbr --add acs "apt search"
        abbr --add ali "apt-mark showmanual"
    case Darwin
        alias agu="brew update && brew upgrade"
        abbr --add agi "brew install"
        abbr --add agr "brew uninstall"
        abbr --add acs "brew search"
        abbr --add ali "brew leaves"
end

### Network services
if type -q ip
    alias ip="ip --color=auto"
    abbr --add myip "ip -4 -brief address"
else
    abbr --add myip "ifconfig | grep inet[^6] | grep -v '127.0.0.1' | cut -d' ' -f2"
end
alias oports="lsof -i 4 -P -n | grep LISTEN | awk '{print \$3,\$1,\$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n | column -t | uniq"
alias serve="python -m SimpleHTTPServer"
