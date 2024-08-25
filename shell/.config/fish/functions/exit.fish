function exit
    jobs -q; and disown (jobs -p)
    builtin exit
end
