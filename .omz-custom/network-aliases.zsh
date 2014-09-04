alias finder_show_hidden="defaults write com.apple.finder AppleShowAllFiles TRUE && killall Finder"
alias finder_hide_hidden="defaults write com.apple.finder AppleShowAllFiles FALSE && killall Finder"

# flush DNS cache
alias dnsreset="sudo killall -HUP mDNSResponder"

# memcache flush
alias flush='echo "flush_all" | nc localhost 11211'

# tool restarts
alias ngrestart="launchctl unload ~/Library/LaunchAgents/homebrew.mxcl.nginx.plist && launchctl load ~/Library/LaunchAgents/homebrew.mxcl.nginx.plist"
alias mysqlrestart="launchctl unload ~/Library/LaunchAgents/homebrew.mxcl.mysql.plist && launchctl load ~/Library/LaunchAgents/homebrew.mxcl.mysql.plist"

# Make zsh know about hosts already accessed by SSH
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

