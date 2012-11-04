c() { cd ~/config/$1; }
_c() { _files -W ~/config -/; }
compdef _c c
