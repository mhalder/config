Prerequisites

    ack plugin: needs ack - p5-app-ack on MacPorts
    fugitive plugin: needs git obviously
    vimpdb: install vimpdb from pypi: pip install vimpdb
    omnicompletion: needs exuberant ctags

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

    ln -s ~/config/bash/ctags ~/.ctags
