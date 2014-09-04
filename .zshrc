# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Defaut path setup
export PATH=/usr/local/share/npm/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:$PATH

# add paths I care about to the execution path
# export PATH=$HOME/bin:$PATH:/usr/local/mysql/bin
export PATH=$HOME/bin:$PATH

# initialize chruby
if [[ -e /usr/local/share/chruby ]]; then
  source /usr/local/share/chruby/chruby.sh
  source /usr/local/share/chruby/auto.sh
  chruby $(cat ~/.ruby-version)
fi

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=7

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# set custom directory
ZSH_CUSTOM=$HOME/.omz-custom

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
#
# had to turn off github plugin due to the following error:
#
# (anon):6: command not found: ___main
# _tags:comptags:36: can only be called from completion function
# _tags:comptry:55: can only be called from completion function
# _tags:comptags:60: can only be called from completion function
# _tags:comptags:67: can only be called from completion function
#

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
plugins=(battery bower brew bundler capistrano chruby coffee colored-man colorize cpanm gem git git-extras git-remote-branch gitfast gitignore gnu-utils heroku history jira lein node npm osx postgres python rails rake redis-cli systemadmin themes thor tmux tmuxinator urltools wd xcode quick_rake zsh-syntax-highlighting phishme vmrun)

fpath=(/usr/local/share/zsh-completions $fpath)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

#export EDITOR='emacsclient -nw'
#export EDITOR=atom
export EDITOR=vim
export ALTERNATE_EDITOR=""

# This resolves issues install the mysql, postgres, and other gems with native non universal binary extensions
# You only want this if you are on Snow Leopard
export ARCHFLAGS='-arch x86_64'
#export CC=/usr/bin/gcc
#export C_INCLUDE_PATH=/usr/local/include

# make zsh completion verbose for debugging purposes
# zstyle ':completion:*' verbose yes
# zstyle ':completion:*:descriptions' format '%B%d%b'
# zstyle ':completion:*:messages' format '%d'
# zstyle ':completion:*:warnings' format 'No matches for: %d'
# zstyle ':completion:*' group-name ''

export TERM=xterm-256color

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

# autojump config
[[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh
