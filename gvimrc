" Set transparency
if has("mac")
    set transp=5
    au FocusLost * set transp=5
    au FocusGained * set transp=5
endif

" Set font
if has("win32")
    set gfn=Courier_New:h9:cANSI
elseif has("mac")
    set gfn=DejaVu\ Sans\ Mono:h11.00
else
    set gfn=Ubuntu\ Mono\ 11.00
endif

" highlight current line, could do crosshair but disturbing
set cursorline
set nocursorcolumn

" Set colorscheme and guioptions
colorscheme xoria256
set go-=m go-=T go-=l go-=L go-=r go-=R go-=b go-=F

" Define sign and and mapping
sign define info text=>> linehl=ErrorMsg texthl=ErrorMsg
map ,ss :exe":sign place 1 line=".line(".")." name=info file=".expand("%:p")<CR>
map ,js :exe":sign jump 1 file=".expand("%:p")<CR>
map ,ds :sign unplace<CR>
