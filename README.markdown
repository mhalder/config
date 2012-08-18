prerequisites

    ack plugin: needs ack (p5-app-ack on macports, ack-grep in ubuntu)
    fugitive plugin: needs git obviously
    vimpdb: install vimpdb from pypi (sudo pip install vimpdb)
    omnicompletion: needs exuberant ctags

installation

    git clone git://github.com/mhalder/config.git ~/config

vim plugins are installed via vundle

    open vim (clones vundle)
    :BundleInstall

create symlinks

    ln -s ~/config/bashrc ~/.bashrc

    ln -s ~/config/vimrc ~/.vimrc
    ln -s ~/config/gvimrc ~/.gvimrc
    ln -s ~/config/vim ~/.vim

    ln -s ~/config/bin ~/bin
