lsd() { cd /u/ls/repos/$1; }
_lsd() { _files -W /u/ls/repos -/; }
compdef _lsd lsd
