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

# Customize to your needs...
PATH=~/bin:$PATH

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/help

export EDITOR=vi
unset VISUAL

eval "$(rbenv init -)"
