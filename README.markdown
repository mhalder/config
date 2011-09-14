Installation

    git clone git://github.com/mhalder/config.git

Vim plugins are installed as git submodules. Run the commands:

    cd config
    git submodule init
    git submodule update

Create symlinks:

    ln -s ~/config/bashrc ~/.bashrc
    ln -s ~/config/vimrc ~/.vimrc
    ln -s ~/config/gvimrc ~/.gvimrc
    ln -s ~/config/vim ~/.vim
    ln -s ~/config/ctags ~/.ctags

Example installation of plugin bundles
---------------------

To install the fugitive plugin, follow these steps:

    cd ~/config
    git submodule add http://github.com/tpope/vim-fugitive.git vim/bundle/fugitive

This will update the `.gitmodules` file by appending something like:

    [submodule "vim/bundle/fugitive"]
        path = vim/bundle/fugitive
        url = http://github.com/tpope/vim-fugitive.git
    
Commit these changes as follows:

    git add .
    git commit -m "added the fugitive bundle"
