#!/bin/zsh

# Variables
export EDITOR="emacsclient"
export PATH=$PATH:$HOME/.local/bin
export TERM=xterm-256color

# Startx when on TTY1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then startx; fi

# Flex on Ubuntu users
neofetch --disable font icons packages resolution theme

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [ ! -f "$HOME/.config/zsh/history" ]; then
   touch $HOME/.config/zsh/history
fi 

if [ ! -d "$HOME/.config/zsh/plugins/powerlevel10k" ]; then
    mkdir $HOME/.config/zsh/plugins/
    git clone https://github.com/romkatv/powerlevel10k $HOME/.config/zsh/plugins/powerlevel10k
fi
   
# Aliases
source $HOME/.config/zsh/config/aliases.zsh

# Completion
autoload -Uz compinit && compinit
zstyle ':completion*' matcher-list 'm:{a-z}={A-Za-z}'

# History
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.config/zsh/history
setopt appendhistory

# Theme
source $HOME/.config/zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme

# Use the vi navigation keys in menu completion
zstyle ':completion:*' menu select
zmodload zsh/complist

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# To customize prompt, run `p10k configure` or edit $HOME/.config/zsh/config/p10k.zsh.
[[ ! -f $HOME/.config/zsh/config/p10k.zsh ]] || source $HOME/.config/zsh/config/p10k.zsh
