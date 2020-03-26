#!/usr/bin/env bash

alacritty -e unimatrix -l k -s 94 &
sleep 0.2

i3-msg fullscreen

i3lock -n; i3-msg kill
