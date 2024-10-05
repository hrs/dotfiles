if status is-interactive
end

set fish_greeting

set -U XDG_DATA_DIRS /usr/local/share:/usr/share

direnv hook fish | source
