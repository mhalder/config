set nocompatible

if !isdirectory(expand("~/.vim/bundle/vundle"))
  !mkdir -p ~/.vim/bundle
  !git clone git://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endif

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" my tpope bundles
Bundle 'tpope/vim-fugitive.git'
Bundle 'tpope/vim-pathogen.git'
Bundle 'tpope/vim-git.git'
Bundle 'tpope/vim-unimpaired.git'
Bundle 'tpope/vim-repeat.git'
Bundle 'tpope/vim-surround.git'
Bundle 'tpope/vim-commentary.git'
Bundle 'tpope/vim-speeddating.git'

" my vim-scripts bundles
Bundle 'vim-scripts/bufexplorer.zip.git'
Bundle 'vim-scripts/tComment.git'
Bundle 'vim-scripts/TaskList.vim.git'

" my derekwyatt bundles
Bundle 'derekwyatt/vim-fswitch.git'
Bundle 'derekwyatt/vim-protodef.git'

" my misc bundles
Bundle 'scrooloose/nerdtree.git'
Bundle 'sjl/gundo.vim.git'
Bundle 'godlygeek/tabular.git'
Bundle 'jceb/vim-orgmode.git'
Bundle 'tsaleh/vim-matchit.git'
Bundle 'mileszs/ack.vim.git'
Bundle 'drmingdrmer/xptemplate.git'
Bundle 'majutsushi/tagbar.git'
Bundle 'ervandew/supertab.git'
Bundle 'gregsexton/gitv.git'
Bundle 'fs111/pydoc.vim.git'
Bundle 'nvie/vim-flake8.git'
Bundle 'kien/ctrlp.vim.git'
Bundle 'Rip-Rip/clang_complete.git'

" add xptemplate global personal directory value
if has("unix")
  set runtimepath+=~/.vim/xpt-personal
endif

" set filetype and syntax stuff to on
filetype on
filetype plugin on
filetype indent on
syntax on

" source abbreviations
:so $HOME/config/vim/abbrev.vim

" set leader to , instead of \
let mapleader=","

" fswitch
au! BufEnter *.cpp let b:fswitchdst = 'hpp' | let b:fswitchlocs = 'rel:include'
au! BufEnter *.hpp let b:fswitchdst = 'cpp' | let b:fswitchlocs = 'rel:..'

" minibufeplorer does not work with fugitive, souce on demand
noremap <silent> <leader>sm :source $HOME/config/vim/source/minibufexpl.vim<CR>
noremap <silent> <leader>em :MiniBufExplorer<CR>

" ctrl-p config
map <leader>cp :CtrlP<CR>
set wildignore+=*/tmp/,*.so,*.swp,*.zip,*.pyc,*.o,*~,*.obj
let g:ctrlp_working_path_mode = 2
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
let g:ctrlp_user_command = {
    \ 'types': {
        \ 1: ['.git/', 'cd %s && git ls-files'],
        \ 2: ['.hg/', 'hg --cwd %s locate -I .'],
        \ 3: ['.svn/', 'cd %s && svn -R list'],
        \ },
    \ 'fallback': 'find %s -type f'
    \ }

" cmake
:autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in runtime! indent/cmake.vim 
:autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in setf cmake
:autocmd BufRead,BufNewFile *.ctest,*.ctest.in setf cmake

" clang_completion
let g:clang_complete_auto = 0
let g:clang_complete_copen = 1
set completeopt=menuone,menu,longest
set pumheight=15

" SuperTab
let g:SuperTabDefaultCompletionType = "context"
"
" TaskList
map <unique> <leader>tl <Plug>TaskList

" omnicompletion for cpp
au BufNewFile,BufRead,BufEnter *.cpp,*.hpp set omnifunc=omni#cpp#complete#Main
noremap <silent> <leader>ct :silent :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" tagbar
noremap <silent> <leader>yy :TagbarToggle<CR>

" minibufexplorer
noremap <silent> <leader>mb :MiniBufExplorer<CR>

" startup window configuration
set lines=50 columns=120

" indentation and desktop config
set cindent
set smartindent
set autoindent
set ruler
" set number relativenumber is cooler
set relativenumber
" use this if you do not use line numbering
" set showbreak=…

" numbering
map <leader>an :set number<CR>
map <leader>rn :set relativenumber<CR>

" set the search scan so that it ignores case when the search is all lower
" case but recognizes uppercase if it's specified
set ignorecase
set smartcase

" tabstops are 4 spaces and expanded
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" enable modeline in files
set modeline

" make command line two lines high
set ch=2

" show special characters, invert with leader-le, spaces with leader-sp and leader-su
if has("win32")
    set listchars=tab:\>\.,eol:$
else
    set listchars=tab:▸\ ,eol:¬
endif
set nolist
nmap <silent> <leader>le :set list!<CR>
nnoremap <silent> <leader>sp :call Preserve("%s/ /./g")<CR>
nnoremap <silent> <leader>su :call Preserve("undo")<CR>

" stop creating backup files
set nobackup

" set the search scan to wrap lines
set wrapscan

" use par as external formatter
set formatprg=par\ -w80req

" set the forward slash to be the slash of note. Backslashes suck
set shellslash

" set visual bell -- i hate that damned beeping
set vb

" allow backspacing over indent, eol, and the start of an insert
set backspace=2

" make sure that unsaved buffers that are to be put in the background are
" allowed to go in there (ie. the "must save first" error doesn't come up)
set hidden

" make the 'cw' and like commands put a $ at the end instead of just deleting
" the text and replacing it
" set cpoptions=ces$

" set the status line the way i like it
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

" don't update the display while executing macros
set lazyredraw

" show the current command in the lower right corner
set showcmd

" show the current mode
set showmode

" show matching parentheses
set showmatch

" hide the mouse pointer while typing
set mousehide

" set up the gui cursor to look nice
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
set timeoutlen=300

" keep some stuff in the history
set history=100

" these commands open folds
set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo

" when the page starts to scroll, keep the cursor 8 lines from the top and 8
" lines from the bottom
set scrolloff=8

" allow the cursor to go in to "invalid" places
" set virtualedit=all

" these things start comment lines
set comments=sl:/*,mb:\ *,ex:\ */,O://,b:#,:%,:XCOMM,n:>,fb:-

" disable encryption (:X)
set key=

" make the command-line completion better
set wildmenu
set wildignore+=*.o,*.obj,*.pyc,*.DS_STORE,*.db,*.swc
set wildmode=longest:full
if has("mac")
    set wildignorecase
endif

" always open the quickfix window when running make, grep, grepadd and vimgrep
autocmd QuickfixCmdPost make,grep,grepadd,vimgrep :botright cwindow

" same as default except that I remove the 'u' option
set complete=.,w,b,t

" when completing by tag, show the whole tag, not just the function name
set showfulltag

" set the textwidth to be 80 chars and wrap during text entry
set textwidth=80
" set formatoptions+=at
" set wrapmargin=5 no effect when textwidth != 0

" enable wrapping
set wrap

" do not break words, does not work with list
set linebreak

" get rid of the silly characters in window separators
set fillchars=""

" Add ignorance of whitespace to diff
set diffopt+=iwhite

" enable search highlighting and incremental search, toggle with leader-th
set hlsearch
set incsearch
nmap <silent> <leader>th :silent :set hlsearch!<CR>

" Toggle spell checking on and off with leader-s
nmap <silent> <leader>s :set spell!<CR>

" setting spell suggestion to only 5 alternatives
set spellsuggest=5
set spelllang=en

" map ESC to jj, quit to leader-jj, save to leader-jk, save quit to leader-jl
imap jj <Esc>
map <leader>jj :q<CR>
map <leader>jk :w<CR>
map <leader>jl :wq<CR>

" enable context search
map <leader>hh <ESC>:exec "help ".expand("<cWORD>")<CR>

" enable Chapter higlight for command file
match ErrorMsg /^* Chapter.*/

" maps to make handling windows a bit easier
noremap <silent> <leader>h :wincmd h<CR>
noremap <silent> <leader>j :wincmd j<CR>
noremap <silent> <leader>k :wincmd k<CR>
noremap <silent> <leader>l :wincmd l<CR>
noremap <silent> <leader>sb :wincmd p<CR>
noremap <silent> <leader>s8 :vertical resize 83<CR>
noremap <silent> <leader>cj :wincmd j<CR>:close<CR>
noremap <silent> <leader>ck :wincmd k<CR>:close<CR>
noremap <silent> <leader>ch :wincmd h<CR>:close<CR>
noremap <silent> <leader>cl :wincmd l<CR>:close<CR>
noremap <silent> <leader>cc :close<CR>
noremap <silent> <leader>cw :cclose<CR>
noremap <silent> <leader>ml <C-W>L
noremap <silent> <leader>mk <C-W>K
noremap <silent> <leader>mh <C-W>H
noremap <silent> <leader>mj <C-W>J
noremap <silent> <C-F9>  :vertical resize -10<CR>
noremap <silent> <C-F10> :vertical resize +10<CR>
noremap <silent> <C-F11> :resize +10<CR>
noremap <silent> <C-F12> :resize -10<CR>

" same for buffers
noremap <silent> <leader>db :bd<CR>

nmap <silent> <leader>ev :tabedit $HOME/config/vimrc<CR>
nmap <silent> <leader>sv :so $HOME/config/vimrc<CR>
nmap <silent> <leader>eg :tabedit $HOME/config/gvimrc<CR>
nmap <silent> <leader>sg :so $HOME/config/gvimrc<CR>
nmap <silent> <leader>vh :tabedit $HOME/config/vim/vimhelp.org<CR>
nmap <silent> <leader>gh :tabedit $HOME/config/git/githelp.org<CR>
nmap <silent> <leader>ea :tabedit $HOME/config/vim/abbrev.vim<CR>

" use reselect visual when indenting in visual mode
vmap > >gv
vmap < <gv

" start scrolling before last line
set scrolloff=3

" search the current file for the word under the cursor and display matches
nmap <silent> <leader>gw :vimgrep /<C-r><C-w>/ %<CR>:ccl<CR>:cwin<CR><C-W>J:nohls<CR>

" toggle the nerd tree on an off with leader-tn
nmap <leader>nt :NERDTreeToggle<CR>

" toggle gundo
nmap <leader>un :GundoToggle<CR>

" bubble single lines
nmap <leader>bu [e
nmap <leader>bd ]e

" bubble multiple lines
vmap <leader>bu [egv
vmap <leader>bd ]egv

" make tab moving easier
nmap <leader>tn :tabnew<CR>
nmap <leader>te :tabedit 
nmap <leader>tf :tabnext<CR>
nmap <leader>tb :tabp<CR>
nmap <leader>tc :tabc<CR>
nmap <leader>on :only<CR>

" visually select the text that was last edited/pasted
nmap gV `[v`]

" cscope mapping for ctrl-_
nmap <C-_>s :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>g :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>c :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>t :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>e :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-_>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-_>d :cs find d <C-R>=expand("<cword>")<CR><CR>

" ctrl-space for horizontal split
nmap <C-Space>s :scs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>g :scs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>c :scs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>t :scs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>e :scs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-Space>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-Space>d :scs find d <C-R>=expand("<cword>")<CR><CR>

" ctrl-space-space for vertical split
nmap <C-Space><C-Space>s
    \:vert scs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>g
    \:vert scs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>c
    \:vert scs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>t
    \:vert scs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>e
    \:vert scs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>i
    \:vert scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-Space><C-Space>d
    \:vert scs find d <C-R>=expand("<cword>")<CR><CR>

" strip trailing whitespaces with leader-sw and format file with leader-ff
nnoremap <silent> <leader>sw :call Preserve("%s/\\s\\+$//e")<CR>
nnoremap <silent> <leader>ff :call Preserve("normal gg=G")<CR>
function! Preserve(command)
    " preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    execute a:command
    " clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

" enable fast opening of files relative to current
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>evs :vsp %%
map <leader>et :tabe %%

" map leader-ls to :ls
map <leader>ls :ls<CR>

" Bufexplorer
map <leader>be :BufExplorer<CR>

" mappings for fugitive
map <leader>gs :Gstatus<CR>
map <leader>gd :Gdiff<CR>
map <leader>gb :Gblame<CR>
map <leader>gl :Glog<CR>
map <leader>gc :Gcommit<CR>
map <leader>ge :Gvsplit :0<CR>
map <leader>gv :Gitv<CR>
map <leader>gf :Gitv!<CR>
map <leader>dp :diffput<CR>
map <leader>dg :diffget<CR>
map <leader>du :diffupdate<CR>

" mappings for xp template
let g:xptemplate_key = '<C-Tab>'

" mappings pep8
noremap <buffer> <leader>pe :call Flake8()<CR>
noremap! <buffer> <leader>pe <Esc>:call Flake8()<CR>

" show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" neat function to replace a string in all occurences in all files
command! -nargs=0 -bar Qargs execute 'args ' . QuickfixFilenames()
function! QuickfixFilenames()
  " Building a hash ensures we get each buffer only once
  let buffer_numbers = {}
  for quickfix_item in getqflist()
    let buffer_numbers[quickfix_item['bufnr']] = bufname(quickfix_item['bufnr'])
  endfor
  return join(values(buffer_numbers))
endfunction

" browse back with .. in git blobs or trees, jump to commit with C
autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif

" auto delete fugitive buffers on close
autocmd BufReadPost fugitive://* set bufhidden=delete
