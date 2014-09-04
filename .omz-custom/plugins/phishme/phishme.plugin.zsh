pvm() { cd /Users/jeffwhitmire/work/vm/$1; }
_pvm() { _files -W /Users/jeffwhitmire/work/vm -/; }
compdef _pvm pvm

pm() { cd /Users/jeffwhitmire/work/$1; }
_pm() { _files -W /Users/jeffwhitmire/work -/; }
compdef _pm pm
