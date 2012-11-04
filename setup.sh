#!/bin/bash

# create src and clone all repos
echo creating src and clone all repos
mkdir ~/src
cd ~/src
git clone git://gitorious.org/evil/evil.git
git clone https://github.com/robbyrussell/oh-my-zsh.git
git clone git://orgmode.org/org-mode.git
git clone https://github.com/bbatsov/prelude.git
git clone http://www.dr-qubit.org/git/undo-tree.git

# creating links
echo creating links

# bash
ln -sf ~/config/bashrc ~/.bashrc 

# git
ln -sf ~/config/git/gitconfig ~/.gitconfig 
ln -sf ~/config/git/gitignore ~/.gitignore 
ln -sf ~/config/git/githelpers ~/.githelpers 

# vim
ln -sf ~/config/gvimrc ~/.gvimrc 
ln -sf ~/config/vimrc ~/.vimrc 
rm -f ~/.vim
ln -sf ~/config/vim ~/.vim 

# zsh
ln -sf ~/config/zsh/zshrc ~/.zshrc 

# bin dir
rm -f ~/bin
ln -sf ~/config/bin ~/bin 
unamestr=`uname`
if [[ "$unamestr" == 'Darwin' ]]; then
    ln -sf ~/bin/mvim ~/bin/gvim
fi

# tmux
ln -sf ~/config/tmux.conf ~/.tmux.conf 

# prelude
ln -sf ~/config/emacs/custom.el ~/src/prelude/personal/custom.el 
rm -f ~/.emacs.d
ln -sf ~/src/prelude ~/.emacs.d

# oh-my-zsh
echo switch to custom branch in oh-my-zsh
cd ~/src/oh-my-zsh
git checkout -b custom

echo creating oh-my-zsh links
ln -sf ~/src/oh-my-zsh ~/.oh-my-zsh
ln -sf ~/config/zsh/custom/mhalder.zsh ~/src/oh-my-zsh/custom/mhalder.zsh
ln -sf ~/config/zsh/custom/plugins ~/src/oh-my-zsh/custom/plugins
ln -sf ~/config/zsh/themes/mhalder.zsh-theme ~/src/oh-my-zsh/themes/mhalder.zsh-theme
rm ~/src/oh-my-zsh/plugins/virtualenvwrapper/virtualenvwrapper.plugin.zsh
ln -sf ~/config/zsh/plugins/virtualenvwrapper/virtualenvwrapper.plugin.zsh ~/src/oh-my-zsh/plugins/virtualenvwrapper/virtualenvwrapper.plugin.zsh
cd ~/src/oh-my-zsh
git add themes/mhalder.zsh-theme
echo committing
git commit --all -m 'customization'
