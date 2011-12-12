#!/bin/bash

# exec xcompmgr -cC -t-5 -l-5 -r4.2 -o.55 &

#feh --bg-scale /home/rolf/documents/pictures/wallpapers/wallpaper-162180.jpg
#feh --bg-scale /home/rolf/documents/pictures/6tOH7.jpg
feh --bg-scale /home/rolf/documents/pictures/wallpapers/4638141721_1d15125b75_o.jpg

xsetroot -cursor_name left_ptr
setxkbmap -layout no
setxkbmap -option terminate:ctrl_alt_bksp
xset +fp /usr/share/fonts/local
xset fp rehash
xmodmap -e 'keycode 35 = 0x7e'

#dropboxd &
(sleep 2s && trayer --edge top --align right --SetDockType true --expand true --width 5 --height 10 --transparent false --tint 0x000000) &
(sleep 2s && wicd-gtk) &

# android-receiver &
#exec ck-launch-session dbus-launch openbox-session

xmonad
