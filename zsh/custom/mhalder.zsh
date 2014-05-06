OS=`uname`

if [ $OS = 'Darwin' ]; then
    alias ec='emacsclient --no-wait'
    alias em='open -a Emacs'

    alias ofi='open . &'
    alias off='open -a Firefox'

    alias l='ls -G -l -h -t' # long, human, color, sort by time
    alias ls='ls -G'
    alias la='ls -G -l -A'   # long, almost all. color
    alias l.='ls -d .[^.]*'

    export PATH=~/bin:/usr/local/bin:$PATH
    export VIM_APP_DIR=/Applications/MacVim

    export LANG=en
else
    alias l='ls --color -l -h -t' # long, human, color, sort by time
    alias ls='ls --color'
    alias la='ls --color -l -A'    # long, almost all. color
    alias l.='ls -d .*'

    alias py='pyclewn --editor=/usr/bin/gvim'

    export PATH=/usr/lib/ccache:~/bin:$PATH
fi

export PATH=~/google-cloud-sdk/bin:~/src/llvm-build/Debug+Asserts/bin:$PATH

alias ea='gvim -f ~/bin/dotfiles/bash/aliases && reload' # edit aliases
alias ee='gvim --remote-silent ~/bin/dotfiles/bash/env'
alias ev='gvim --remote-silent ~/bin/vim/vim-commands.txt'
alias gv='gvim --remote-silent'
alias gw='gvim -f'
alias gvs='gvim --remote-silent .git/index'
alias vs='vim .git/index'

alias c='clear'
alias cl='clear; l'
alias cls='clear; ls'

alias md='mkdir -p'
alias s='cd ..'
alias e='exit'

alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--|/g' -e 's/^/ /' -e 's/-/|/'"

# processes
alias tu='top -o cpu'   # cpu
alias tm='top -o vsize' # memory

# git
alias ungit="find . -name '.git' -exec rm -rf {} \;"
alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -d'
alias gc='git commit -v'
alias gca='git commit -v -a'

alias gco='git checkout'
alias gd='git diff'
alias gdm='git diff master'
alias gl='git pull'
alias glo='git pull origin master'
alias gp='git push'
alias gpo='git push origin master'
alias g='git status'
alias gk='git --no-pager log --pretty=oneline --all --decorate -30 --graph --abbrev-commit'
alias gkb='git --no-pager log --pretty=oneline --decorate -30 --graph --abbrev-commit'
alias gkn='git log --pretty=oneline --all --decorate --graph --abbrev-commit'

# push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# basic directory operations
alias ...='cd ../..'

alias _='sudo'

# show history
alias history='fc -l 1'

# cool grep
alias afind='ack-grep -il'
alias ffind='find . -name'

unsetopt correct_all

# gcloud completion
if [ -f ~/google-cloud-sdk/completion.zsh.inc ]; then
    source ~/google-cloud-sdk/completion.zsh.inc
fi

export LC_CTYPE=en_US.UTF-8
