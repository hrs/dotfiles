# .xinitrc

# fire up the dropbox daemon here, why not
~/.dropbox-dist/dropboxd &

# don't clone displays
# xrandr --output HDMI-0 --auto --left-of DVI-0
xrandr --output DVI-0 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-0 --left-of DVI-0 --mode 1920x1080 --pos 0x0 --rotate normal

feh --bg-tile ~/.wallpaper/background-tile.png
setxkbmap -option ctrl:nocaps
xsetroot -cursor_name left_ptr

xmonad
