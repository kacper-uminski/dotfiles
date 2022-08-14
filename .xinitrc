eval $(gnome-keyring-daemon --start)
export SSH_AUTH_LOCK
#export MESA_LOADER_DRIVER_OVERRIDE=i965
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=kvantum
xrandr --output DP2 --rotate right --left-of DP1 &&
exec xmonad
