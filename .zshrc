if [ -n "$INSIDE_EMACS" ]; then
    export ZSH_THEME="clean"
else
    export ZSH_THEME="muse"
fi

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=7

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# set custom directory
ZSH_CUSTOM=$HOME/.omz-custom

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(brew brew-cask bundler colored-man colorize gem git git-extras git-remote-branch gitfast gitignore history jira postgres python rails rake ssh-agent systemadmin themes tmux tmuxinator zsh_reload zsh-syntax-highlighting phishme)

source $ZSH/oh-my-zsh.sh

# User configuration

# add cask support for emacs
export PATH="/home/deploy/.cask/bin:$PATH"

export PATH=$HOME/bin:/usr/local/bin:$PATH



export EDITOR=vim
export ALTERNATE_EDITOR=""

# set the prompt to plain if a dumb terminal (used by emacs/tramp)
# [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
[ $TERM != "xterm-256color" ] && unsetopt zle && PS1='$ '
# unsetopt zle && PS1='$ '

# make zsh completion verbose for debugging purposes
# zstyle ':completion:*' verbose yes
# zstyle ':completion:*:descriptions' format '%B%d%b'
# zstyle ':completion:*:messages' format '%d'
# Customize to your needs...
# zstyle ':completion:*:warnings' format 'No matches for: %d'
# zstyle ':completion:*' group-name ''
