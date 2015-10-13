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
path=("$HOME/bin" $path)

#
# Customizations
#
alias gst='git status'
compdef _git gst=git-status
export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"

export GOPATH=~/dev/go
export PATH=$PATH:$GOPATH/bin

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/help

export PATH="$HOME/.rvm/bin:$PATH" # Add RVM to PATH for scripting
