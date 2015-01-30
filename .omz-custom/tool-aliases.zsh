# GREP_COLOR=bright yellow on black bg.
# use GREP_COLOR=7 to highlight whitespace on black terminals
# LANG=C for speed. See also: http://www.pixelbeat.org/scripts/findrepo
alias grep='GREP_COLOR="1;33;40" LANG=C grep --color=auto'

# ls aliases.  Stolen and adapted from the common_aliases zsh plugin
alias ls='ls -G'         # show colorized version
alias l='ls -lFhG'       # size,show type,human readable
alias la='ls -lAFhG'     # long list,show almost all,show type,human readable
alias lr='ls -tRFhG'     # sorted by date,recursive,show type,human readable
alias lt='ls -ltFhG'     # long list,sorted by date,show type,human readable
alias ll='ls -lG'        # long list
alias ldot='ls -ldG .*'  # show all dotfiles
alias lS='ls -1FSshG'    # 1 column, sorted by size
alias lart='ls -1FcartG' # 1 column; all files; sorted by time
alias lrt='ls -1FcrtG'   # 1 column; sorted by size

# because typing 'cd' is A LOT of work!!
alias ..='cd ../'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'

alias tf='tail -f'

alias dud='du --max-depth=1 -h'
alias duf='du -sh *'
alias fd='find . -type d -name'
alias ff='find . -type f -name'

# memcache flush
alias flush='echo "flush_all" | nc localhost 11211'
alias resetdns='dscacheutil -flushcache;sudo killall -HUP mDNSResponder'

# puppet
alias papply='sudo puppet apply --hiera_config=/opt/phishme/puppet/hiera/hiera.yaml --modulepath=/opt/phishme/puppet/modules /opt/phishme/puppet/manifests/triage.pp -l /opt/phishme/puppet/puppet.log'
