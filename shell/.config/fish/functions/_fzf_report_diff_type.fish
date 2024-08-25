# helper for _fzf_preview_changed_file
# prints out something like
# ╭────────╮
# │ Staged │
# ╰────────╯
function _fzf_report_diff_type --argument-names diff_type --description "Print a distinct colored header meant to preface a git patch."
    # number of "-" to draw is the length of the string to box + 2 for padding
    set -f repeat_count (math 2 + (string length $diff_type))
    set -f line (string repeat --count $repeat_count ─)
    set -f top_border ╭$line╮
    set -f btm_border ╰$line╯

    set_color yellow
    echo $top_border
    echo "│ $diff_type │"
    echo $btm_border
    set_color normal
end
