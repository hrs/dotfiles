function fish_vterm_prompt_end
    printf '\e]51;A'(pwd)'\e\\'
end

function fish_prompt
    printf "[%s:%s] \$ " $hostname (prompt_pwd)
    fish_vterm_prompt_end
end
