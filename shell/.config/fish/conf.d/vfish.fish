# VTerm (https://github.com/akermu/emacs-libvterm#shell-side-configuration)
if test 'vterm' = "$INSIDE_EMACS" \
    -a -n "$EMACS_VTERM_PATH" \
    -a -f "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"

    # vterm does not work with tide, unfortunately
    if functions --query tide
        function fish_prompt -d "Write out the prompt"
            printf '%s@%s%s%s%s> ' (whoami) (hostname | cut -d . -f 1) \
                (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
        end
    end

    # Clean up old junk functions just in case.
    functions --erase vterm_old_fish_prompt

    # Load the setup that comes with vterm.
    source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"

    function vf --description 'Open a file for editing in Emacs from vterm'
        vterm_cmd find-file (realpath "$argv")
    end

    function vd --description 'Run dired on a directory from vterm'
        set -f dir "$argv"
        if test "x" = "x$dir"
            set dir (pwd)
        end
        vterm_cmd dired (realpath "$dir")
    end

    function vdiff --argument-names filea fileb --description 'Run ediff-files on files A and B'
        vterm_cmd ediff-files (realpath "$filea") (realpath "$fileb")
    end

    function vz --description 'Use "z" to run dired directly from vterm'
        if functions --query __z
            set -l dir (z -e "$argv[1]")
            vterm_cmd dired (realpath "$dir")
        else
            echo "Install z for fish shell from https://github.com/jethrokuan/z to use this command."
            return 1
        end
    end
end
