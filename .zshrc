#!/bin/zsh
# Execute startx if in tty1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then exec startx; fi

# Flex on Arch users!
neofetch

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Aliases
source $HOME/.config/zsh/config/aliases.zsh

# Completion
autoload -Uz compinit && compinit
zstyle ':completion*' matcher-list 'm:{a-z}={A-Za-z}'

# History
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.config/zsh/history/zsh_history
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

# Variables
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export PATH="$PATH:$HOME/.local/bin:$HOME/.cargo/bin"
export TERM=xterm-256color

# To customize prompt, run `p10k configure` or edit $HOME/.config/zsh/config/p10k.zsh.
[[ ! -f $HOME/.config/zsh/config/p10k.zsh ]] || source $HOME/.config/zsh/config/p10k.zsh
