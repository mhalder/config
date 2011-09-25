Prerequisites

    ack plugin: needs ack - p5-app-ack on MacPorts
    fugitive plugin: needs git obviously

Installation

    git clone git://github.com/mhalder/config.git ~/config

Vim plugins are installed as git submodules. Run the commands:

    cd ~/config
    git submodule update --init

Create symlinks

    ln -s ~/config/bashrc ~/.bashrc

    ln -s ~/config/vimrc ~/.vimrc
    ln -s ~/config/gvimrc ~/.gvimrc
    ln -s ~/config/vim ~/.vim

    ln -s ~/config/bin ~/bin

    ln -s ~/config/ctags ~/.ctags

To install the fugitive plugin, follow these steps:

    cd ~/config
    git submodule add http://github.com/tpope/vim-fugitive.git vim/bundle/fugitive

This will update the '.gitmodules' file by appending something like:

    [submodule "vim/bundle/fugitive"]
        path = vim/bundle/fugitive
        url = http://github.com/tpope/vim-fugitive.git
    
Commit these changes as follows:

    git add .
    git commit -m "added the fugitive bundle"
