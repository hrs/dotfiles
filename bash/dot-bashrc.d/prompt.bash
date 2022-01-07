# -*- mode: sh -*-

# Show a * and/or + after the branch name if there are unstaged or staged
# changes.
GIT_PS1_SHOWDIRTYSTATE=true

PS1='\[\033[36m\][\w$(__git_ps1)] \$ \[\033[00m\]'
