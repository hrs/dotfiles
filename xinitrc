# .xinitrc

# ~/.dropbox-dist/dropboxd &

# don't clone displays
# xrandr --output HDMI-0 --auto --left-of DVI-0
# xrandr --output DVI-0 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-0 --left-of DVI-0 --mode 1920x1080 --pos 0x0 --rotate normal

xrandr --output VBOX0 --mode 1600x1200

feh --bg-tile ~/.wallpaper/light-linen.png
setxkbmap -option ctrl:nocaps
xsetroot -cursor_name left_ptr

xmonad
