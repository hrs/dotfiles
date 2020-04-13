# -*- mode: sh -*-

GIT_PS1_SHOWDIRTYSTATE=true

# __git_ps1 reports "((unknown))" in directories with broken git
# repositories; unfortunately, pre-commit hooks are generally kept in
# an empty git repo in $HOME, resulting in unneeded grossness.
__quiet_git_ps1() {
  local b="$(__git_ps1)"
  if [ "$b" != " ((unknown))" ]; then
    echo -n "$b"
  fi
}

PS1='\[\033[36m\][\w$(__quiet_git_ps1)] \$ \[\033[00m\]'
