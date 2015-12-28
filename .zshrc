#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#
# Customizations
#

alias gst='git status'
compdef _git gst=git-status

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/help

export EDITOR=vi
unset VISUAL
