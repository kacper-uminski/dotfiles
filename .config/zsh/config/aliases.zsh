#!/bin/zsh
alias dotfiles='/usr/bin/git --git-dir=$HOME/documents/dotfiles --work-tree=$HOME'
alias eupdate='doas emerge --sync && doas emerge -auDN --with-bdeps=y @world'
alias ls='exa -al'
alias minecraft-launcher='$HOME/software/minecraft-launcher/minecraft-launcher'
alias th06='WINEPREFIX=~/software/touhou/TH06/wine WINEARCH=win32 LANG="en_US.UTF-8" wine ~/software/touhou/TH06/Touhou\ 6\ \ -\ The\ Embodiment\ of\ Scarlet\ Devil/Touhou06.exe'
alias vim='nvim'
