set nocompatible

" load and call pathogen
source ~/config/vim/bundle/pathogen/autoload/pathogen.vim
call pathogen#infect('~/config/vim/bundle')

" set filetype and syntax stuff to on
filetype on
filetype plugin on
filetype indent on
syntax on

" source abbreviations
:so ~/config/abbrev.vim

" set leader to , instead of \
let mapleader=","

" startup window configuration
set lines=50 columns=120

" indentation and deskopt config
set cindent
set smartindent
set autoindent
set ruler
" set number relativenumber is cooler
set relativenumber
" use this if you do not use line numbering
" set showbreak=…

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
set timeoutlen=500

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
set wildignorecase

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
map <F1> <ESC>:exec "help ".expand("<cWORD>")<CR>

" enable Chapter higlight for command file
match ErrorMsg /^* Chapter.*/

" maps to make handling windows a bit easier
noremap <silent> <leader>h :wincmd h<CR>
noremap <silent> <leader>j :wincmd j<CR>
noremap <silent> <leader>k :wincmd k<CR>
noremap <silent> <leader>l :wincmd l<CR>
noremap <silent> <leader>sb :wincmd p<CR>
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

nmap <silent> <leader>ev :tabedit $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>eg :tabedit $MYGVIMRC<CR>
nmap <silent> <leader>sg :so $MYGVIMRC<CR>
nmap <silent> <leader>eh :tabedit ~/config/vim.org<CR>
nmap <silent> <leader>ea :tabedit ~/config/abbrev.vim<CR>

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
nmap <leader>gu :GundoToggle<CR>

" bubble single lines
nmap <leader>bu [e
nmap <leader>bd ]e

" bubble multiple lines
vmap <leader>bmu [egv
vmap <leader>bmd ]egv

" make tab moving easier
nmap <leader>tn :tabnew<CR>
nmap <leader>te :tabedit 
nmap <leader>tf :tabnext<CR>
nmap <leader>tb :tabp<CR>
nmap <leader>tc :tabc<CR>

" visually select the text that was last edited/pasted
nmap gV `[v`]

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

" Show syntax highlighting groups for word under cursor
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

" org-table like behaviour
inoremap <Bar>   <Bar><Esc>:call <SID>align()<CR>a
function! <SID>align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction
