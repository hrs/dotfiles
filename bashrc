#!/bin/bash

source ~/.bin/git-completion.sh

### Adjusting the PATH
if [[ $(uname) == Darwin ]]; then
    emacs_path=/Applications/Emacs.app/Contents/MacOS/bin
    M2_HOME=$HOME/Documents/tools/maven
    export PATH=$emacs_path:$PATH
fi
conf_dot_home_path=$HOME/.bin
tex_path=/usr/local/texlive/2011/bin/x86_64-darwin
# conf_rvm_path="/home/hrs/.rvm/bin"
export PATH=/usr/local/bin:/usr/local/sbin:$PATH:/sbin:$tex_path:$conf_dot_home_path
export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules

if [[ -n "$M2_HOME" ]]; then
    export PATH=$M2_HOME/bin:$PATH
fi

### Setting variables
export DISPLAY=:0.0
export EDITOR="emacsclient -nw -c -a ''"
export LANG="en_US"
export LC_ALL=C

### Assorted aliases
alias ack="ack-grep"
alias clr="rm -f *~ .*~"
alias e=$EDITOR
alias gg="git grep -n"
alias la="ls -la"
alias less="less -R" # display colors correctly
alias lisp="sbcl --noinform"
alias lispi="sbcl -noinform --load"
alias ll="ls -l"
alias ln="ln -v"
alias mkdir="mkdir -p"
alias n="e ~/Documents/notes"
alias now="date '+%Y-%m-%d %H:%M'"
alias path="echo $PATH | tr ':' '\n'"
alias sbcl="rlwrap sbcl"
alias texclean="rm *.aux *.log *.bbl *.blg *.pdf"
alias tree="tree -C" # add colors
alias unb="tar xjvf"
alias ung="tar xzvf"
alias usage="du -sch"

todo_queue="$HOME/Dropbox/org/to-process.org"
todo_prefix="** TODO"

# create org-formatted todos
function todo {
    echo "$todo_prefix $@" >> $todo_queue
}

# display todos in the to-process queue
alias todos="sed s/'$todo_prefix '// < $todo_queue"

alias etodos="$EDITOR $todo_queue"

# Override the mvn command with the colorized one.
alias mvn="mvn-color"

function java6 () {
    export JAVA_HOME=`/usr/libexec/java_home -v 1.6`
}

function java7 () {
    export JAVA_HOME=`/usr/libexec/java_home -v 1.7`
}

### Aiming for something like platform-independence
if [[ $(uname) == Darwin ]]; then
    alias op="open"
    alias ls="ls -G -h"
    alias myip="ifconfig | grep 'inet ' | grep -v 127.0.0.1 | cut -d\   -f2"
    alias nowp="now | pbcopy"
    alias generate_rails_tags="rm -f TAGS; ctags -a -e --Ruby-kinds=-fF -o TAGS -R app lib vendor ."
else # Linux!
    alias op="gnome-open"
    alias ls="ls --color -h"
    alias myip="ifconfig eth0 | grep 'inet ' | cut --delimiter=' ' -f12 | sed s/addr://"
    alias generate_rails_tags="rm -f TAGS; ctags-exuberant -a -e -f TAGS --tag-relative -R app lib vendor"
fi

### Package management
if [[ $(uname) == Darwin ]]; then
    alias agi="brew install"
    alias agr="brew rm"
    alias acs="brew search"
    alias agu="brew update && brew upgrade"
else
    alias agi="sudo apt-get install"
    alias agr="sudo apt-get remove"
    alias acs="apt-cache search"
    alias agu="sudo apt-get update && sudo apt-get upgrade && sudo apt-get dist-upgrade"
fi

alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias serve="python -m SimpleHTTPServer"
alias kindle="rsync -r -v ~/Documents/books/kindle/* /Volumes/Kindle/documents"

### Various useful functions
function countpage() {
    pdf2dsc "$1" /dev/stdout | grep "Pages" | sed s/[^0-9]//g
}

function viewpages() {
    for file in "$@"; do
        filepagecount=$(countpage "$file")
        printf "%4d %s\n" $filepagecount $file
    done
}

### Meta
alias rc="$EDITOR ~/.bashrc"
alias rcs="source ~/.bashrc"

### Adjust prompt
GIT_PS1_SHOWDIRTYSTATE=true

# __git_ps1 reports "((unknown))" in directories with broken git repositories;
# unfortunately, pre-commit hooks are generally kept in an empty git repo in $HOME,
# resulting in unneeded grossness.
__quiet_git_ps1() {
    local b="$(__git_ps1)"
    if [ "$b" != " ((unknown))" ]; then
        echo -n "$b"
    fi
}

PS1='\[\033[36m\][\w$(__quiet_git_ps1)] \$ \[\033[00m\]'

### Launch the shell!

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ -s $HOME/.nvm/nvm.sh ]] && . $HOME/.nvm/nvm.sh # This loads NVM

### loading rvm
source $HOME/.rvm/scripts/rvm
