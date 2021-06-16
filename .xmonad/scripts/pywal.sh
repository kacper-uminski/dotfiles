
#!/bin/bash

input="$HOME/.cache/wal/colors.yml"
# yq is a command line yaml parser which allows querring yaml with a selector

FOREGROUND=$(yq -r .special.foreground $HOME/.cache/wal/colors.yml)
BACKGROUND=$(yq -r .special.background $HOME/.cache/wal/colors.yml)

COLOR0=$(yq -r .colors.color0 $HOME/.cache/wal/colors.yml)
COLOR1=$(yq -r .colors.color1 $HOME/.cache/wal/colors.yml)
COLOR2=$(yq -r .colors.color2 $HOME/.cache/wal/colors.yml)
COLOR3=$(yq -r .colors.color3 $HOME/.cache/wal/colors.yml)
COLOR4=$(yq -r .colors.color4 $HOME/.cache/wal/colors.yml)
COLOR5=$(yq -r .colors.color5 $HOME/.cache/wal/colors.yml)
COLOR6=$(yq -r .colors.color6 $HOME/.cache/wal/colors.yml)
COLOR7=$(yq -r .colors.color7 $HOME/.cache/wal/colors.yml)
COLOR8=$(yq -r .colors.color8 $HOME/.cache/wal/colors.yml)
COLOR9=$(yq -r .colors.color9 $HOME/.cache/wal/colors.yml)
COLOR10=$(yq -r .colors.color10 $HOME/.cache/wal/colors.yml)
COLOR11=$(yq -r .colors.color11 $HOME/.cache/wal/colors.yml)
COLOR12=$(yq -r .colors.color12 $HOME/.cache/wal/colors.yml)
COLOR13=$(yq -r .colors.color13 $HOME/.cache/wal/colors.yml)
COLOR14=$(yq -r .colors.color14 $HOME/.cache/wal/colors.yml)
COLOR15=$(yq -r .colors.color15 $HOME/.cache/wal/colors.yml)

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xpm-template/calendar-clock-icon_20.xpm > $HOME/.xmonad/xpm/calendar-clock-icon_20.xpm

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xpm-template/cpu_20.xpm > $HOME/.xmonad/xpm/cpu_20.xpm

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xpm-template/harddisk-icon_20.xpm > $HOME/.xmonad/xpm/harddisk-icon_20.xpm

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xpm-template/haskell_20.xpm > $HOME/.xmonad/xpm/haskell_20.xpm

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xpm-template/memory-icon_20.xpm > $HOME/.xmonad/xpm/memory-icon_20.xpm

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xpm-template/net_down_20.xpm > $HOME/.xmonad/xpm/net_down_20.xpm

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xpm-template/net_up_20.xpm > $HOME/.xmonad/xpm/net_up_20.xpm

sed -e "s/FGCOLOR/$FOREGROUND/g" \
    -e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.config/xmobar/xmobarrc-template > $HOME/.config/xmobar/xmobarrc

sed -e "s/FGCOLOR/$FOREGROUND/g" \
	-e "s/BGCOLOR/$BACKGROUND/g" \
    -e "s/COLOR1/$COLOR1/g" \
    -e "s/COLOR2/$COLOR2/g" \
    -e "s/COLOR3/$COLOR3/g" \
    -e "s/COLOR4/$COLOR4/g" \
    -e "s/COLOR5/$COLOR5/g" \
    -e "s/COLOR6/$COLOR6/g" \
    -e "s/COLOR7/$COLOR7/g" \
    -e "s/COLOR8/$COLOR8/g" \
    -e "s/COLOR9/$COLOR9/g" \
    -e "s/COLOR10/$COLOR10/g" \
    -e "s/COLOR11/$COLOR11/g" \
    -e "s/COLOR12/$COLOR12/g" \
    -e "s/COLOR13/$COLOR13/g" \
    -e "s/COLOR14/$COLOR14/g" \
    -e "s/COLOR15/$COLOR15/g" \
    $HOME/.xmonad/xmonad-template.hs > $HOME/.xmonad/xmonad.hs

killall xmobar
xmonad --recompile; xmonad --restart


