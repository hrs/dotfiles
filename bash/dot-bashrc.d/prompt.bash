# -*- mode: sh -*-

# Show a * and/or + after the branch name if there are unstaged or staged
# changes.
GIT_PS1_SHOWDIRTYSTATE=true

PS1='[\h:\w$(__git_ps1)] \$ '
