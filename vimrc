" we want the real vim
set nocompatible

" call pathogen scripts
filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Set filetype stuff to on
filetype on
filetype plugin on
filetype indent on
syntax on

" source abbreviations
if has("win32")
  :so ~/vimfiles/abbrev.vim
else
  :so ~/.vim/abbrev.vim
endif

" set leader to , instead of \
let mapleader = ","

" startup window configuration
set lines=50 columns=120

" indentation and deskopt config
set cindent
set smartindent
set autoindent
set ruler
set number

" Tabstops are 4 spaces and expanded
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" Enable modeline in files
set modeline

" Show special characters, invert with leader-le, spaces with leader-sp and leader-su
" set listchars=tab:\>\.,eol:$
set listchars=tab:▸\ ,eol:¬
set nolist
nmap <silent> <leader>le :set list!<CR>
nnoremap <silent> <leader>sp :call Preserve("%s/ /./g")<CR>
nnoremap <silent> <leader>su :call Preserve("undo")<CR>

" stop creating backup files
set nobackup

" set the search scan to wrap lines
set wrapscan

" set the search scan so that it ignores case when the search is all lower
" case but recognizes uppercase if it's specified
set ignorecase
set smartcase

" set the forward slash to be the slash of note. Backslashes suck
set shellslash

" Make command line two lines high
set ch=2

" set visual bell -- i hate that damned beeping
set vb

" Allow backspacing over indent, eol, and the start of an insert
set backspace=2

" Make sure that unsaved buffers that are to be put in the background are
" allowed to go in there (ie. the "must save first" error doesn't come up)
set hidden

" Make the 'cw' and like commands put a $ at the end instead of just deleting
" the text and replacing it
" set cpoptions=ces$

" Set the status line the way i like it
" set stl=%f\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]
set stl=
set stl+=%f:
set stl+=%m
set stl+=%r
set stl+=Line%lof%L[%p%%]
set stl+=%{fugitive#statusline()}
set stl+=%=
set stl+=Col%c:
set stl+=Buf%n:
set stl+=[%b][0x%B]
set stl+=%#warningmsg#
set stl+=%*

" tell VIM to always put a status line in, even if there is only one window
" switch off with laststatus=0
set laststatus=2

" Don't update the display while executing macros
set lazyredraw

" Show the current command in the lower right corner
set showcmd

" Show the current mode
set showmode

" Show matching parentheses
set showmatch

" Switch on syntax highlighting.
syntax on

" Hide the mouse pointer while typing
set mousehide

" Set up the gui cursor to look nice
set guicursor=n-v-c:block-Cursor-blinkon0
set guicursor+=ve:ver35-Cursor
set guicursor+=o:hor50-Cursor
set guicursor+=i-ci:ver25-Cursor
set guicursor+=r-cr:hor20-Cursor
set guicursor+=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175

" set the gui options the way I like
set guioptions=ac

" This is the timeout used while waiting for user input on a multi-keyed macro
" or while just sitting and waiting for another key to be pressed measured
" in milliseconds.
"
" i.e. for the ",d" command, there is a "timeoutlen" wait period between the
"      "," key and the "d" key.  If the "d" key isn't pressed before the
"      timeout expires, one of two things happens: The "," command is executed
"      if there is one (which there isn't) or the command aborts.
set timeoutlen=500

" Keep some stuff in the history
set history=100

" These commands open folds
set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo

" When the page starts to scroll, keep the cursor 8 lines from the top and 8
" lines from the bottom
set scrolloff=8

" Allow the cursor to go in to "invalid" places
" set virtualedit=all

" These things start comment lines
set comments=sl:/*,mb:\ *,ex:\ */,O://,b:#,:%,:XCOMM,n:>,fb:-

" Disable encryption (:X)
set key=

" Make the command-line completion better
set wildmenu
set wildignore+=*.o,*.obj,*.pyc,*.DS_STORE,*.db,*.swc
set wildmode=longest:full

" Always open the quickfix window when running make, grep, grepadd and vimgrep
autocmd QuickfixCmdPost make,grep,grepadd,vimgrep :botright cwindow

" Same as default except that I remove the 'u' option
set complete=.,w,b,t

" When completing by tag, show the whole tag, not just the function name
set showfulltag

" Set the textwidth to be 120 chars
set textwidth=120

" get rid of the silly characters in window separators
set fillchars=""

" Add ignorance of whitespace to diff
set diffopt+=iwhite

" Enable search highlighting and incremental search, toggle with leader-th
set hlsearch
set incsearch
nmap <silent> <leader>th :silent :set hlsearch!<CR>

" Setting spell suggestion to only 5 alternatives
set spellsuggest=5

" Map ESC to jj, quit to <leader>-jj, save to <leader>-jk, save quit to <leader>-jl
imap jj <Esc>
map <leader>jj :q<CR>
map <leader>jk :w<CR>
map <leader>jl :wq<CR>

" Enable context search
map <F1> <ESC>:exec "help ".expand("<cWORD>")<CR>

" Enable Chapter higlight for command file
match ErrorMsg /^Chapter/

" Maps to make handling windows a bit easier
noremap <silent> ,h :wincmd h<CR>
noremap <silent> ,j :wincmd j<CR>
noremap <silent> ,k :wincmd k<CR>
noremap <silent> ,l :wincmd l<CR>
noremap <silent> ,sb :wincmd p<CR>
noremap <silent> <C-F9>  :vertical resize -10<CR>
noremap <silent> <C-F10> :resize +10<CR>
noremap <silent> <C-F11> :resize -10<CR>
noremap <silent> <C-F12> :vertical resize +10<CR>
noremap <silent> ,s8 :vertical resize 83<CR>
noremap <silent> ,cj :wincmd j<CR>:close<CR>
noremap <silent> ,ck :wincmd k<CR>:close<CR>
noremap <silent> ,ch :wincmd h<CR>:close<CR>
noremap <silent> ,cl :wincmd l<CR>:close<CR>
noremap <silent> ,cc :close<CR>
noremap <silent> ,cw :cclose<CR>
noremap <silent> ,ml <C-W>L
noremap <silent> ,mk <C-W>K
noremap <silent> ,mh <C-W>H
noremap <silent> ,mj <C-W>J
noremap <silent> <C-7> <C-W>>
noremap <silent> <C-8> <C-W>+
noremap <silent> <C-9> <C-W>+
noremap <silent> <C-0> <C-W>>

" Edit the vimrc file
if has("win32")
  nmap <silent> ,ev :e ~/vimfiles/vimrc<CR>
  nmap <silent> ,sv :so ~/vimfiles/vimrc<CR>
  nmap <silent> ,eg :e ~/vimfiles/gvimrc<CR>
  nmap <silent> ,sg :so ~/vimfiles/gvimrc<CR>
  nmap <silent> ,ec :e ~/vimfiles/myhelp.txt<CR>
  nmap <silent> ,ea :e ~/vimfiles/abbrev.vim<CR>
else
  nmap <silent> ,ev :e ~/.vimrc<CR>
  nmap <silent> ,sv :so ~/.vimrc<CR>
  nmap <silent> ,eg :e ~/.gvimrc<CR>
  nmap <silent> ,sg :so ~/.gvimrc<CR>
  nmap <silent> ,ec :e ~/.vim/myhelp.txt<CR>
  nmap <silent> ,ea :e ~/.vim/abbrev.vim<CR>
endif

" Use reselect visual when indenting in visual mode
vmap > >gv
vmap < <gv

" set search path for gf command, not verified
" set path=.,/usr/include,/usr/local/include,**;$HOME

" start scrolling before last line
set scrolloff=3

" strip trailing whitespaces with leader-sw and format file with leader-ff
nnoremap <silent> <leader>sw :call Preserve("%s/\\s\\+$//e")<CR>
nnoremap <silent> <leader>ff :call Preserve("normal gg=G")<CR>
function! Preserve(command)
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    execute a:command
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction
