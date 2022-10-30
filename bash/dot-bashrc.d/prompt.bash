# -*- mode: sh -*-

# Show a * and/or + after the branch name if there are unstaged or staged
# changes.
GIT_PS1_SHOWDIRTYSTATE=true

PS1='\[\033[38;5;91m\][\h:\w$(__git_ps1)] \$ \[\033[00m\]'
