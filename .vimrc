syntax on

if &t_Co >= 256
        colorscheme fu
else
        colorscheme zenburn
endif

" let mapleader=","

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

set nocompatible
set laststatus=2
set statusline=
set statusline+=%-3.3n\                      " buffer number
set statusline+=%f\                          " filename
set statusline+=%h%m%r%w                     " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " filetype
set statusline+=%=
set statusline+=%-14(%l,%c%V%)
set statusline+=%<%P
set history=1000
set undolevels=1000
set title

if version >= 700
  au InsertEnter * hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=Blue
  au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
endif

set hlsearch  " highlight search
set incsearch " incremental search, search as you type
set smartcase " Ignore case when searching lowercase

set expandtab
set tabstop=8
set shiftwidth=8
set smarttab
set backspace=indent,eol,start
set showcmd
set hidden
set autoindent
set copyindent

set nowrap
set linebreak " Wrap at word
set number
set vb t_vb=  " turn off bell!
set dir=~/.vim/tmp
highlight LineNr ctermfg=lightcyan

" imap jj <Esc>

filetype indent plugin on
" set cindent

"au BufWinLeave juleøltest.txt mkview
"au BufWinEnter juleøltest.txt silent loadview

" au BufWritePre 2009.nanowrimo let &bex = '-' . strftime("%F-%H.%M.%S") . '~'
" au BufWinEnter 2009.nanowrimo silent loadview
" au BufWinLeave 2009.nanowrimo mkview
" au BufRead,BufNewFile *.nanowrimo set filetype=nanowrimo
" au! Syntax nanowrimo source /home/rolf/.vim/syntax/nanowrimo.vim

nnoremap <space> za

map  <F2> :tabe %:p:s,.h$,.X123X,:s,.c$,.h,:s,.X123X$,.c,<CR>
imap <F3> <C-R>=strftime("%Y-%m-%d %H:%M")<CR>
nmap <F3> o<C-R>=strftime("%Y-%m-%d %H:%M")<CR><Esc>
nmap <F7> g<C-G>
map <F5> :make<CR>

map <left>  :tabprevious<CR>
map <right> :tabnext<CR>
map <down> :bprevious<CR>
map <up> :bnext<CR>

" Convenient go-to-end-of-line-key on norwegian keyboard.
map \ $

" Put norwegian keys to use :)
map ø :
map æ @

map gr gT
map g> :%s/>/->/g<CR>

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

noremap <C-s> <C-a>
nmap <silent> ,l :nohlsearch<CR>

" programming shortcuts/stuff
inoremap { {<CR>}<ESC>O
inoremap {<CR> {
inoremap (<CR> ()<Left>

" Autocommands
" " Read-only .doc through antiword
autocmd BufReadPre *.doc silent set ro
autocmd BufReadPost *.doc silent %!antiword "%"
"
" " Read-only odt/odp through odt2txt
autocmd BufReadPre *.odt,*.odp silent set ro
autocmd BufReadPost *.odt,*.odp silent %!odt2txt "%"

au FileType help nnoremap <buffer><cr> <C-]> " Enter selects subject
au FileType help nnoremap <buffer><bs> <C-T> " Backspace to go back

autocmd FileType c set omnifunc=ccomplete#Complete

" Super Tab Completion stuff
function! SuperCleverTab()
        if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
                return "\<Tab>"
        else
                if &omnifunc != ''
                        if &filetype == "c" || &filetype == "cpp"
                            return "\<C-N>"
                        else
                            return "\<C-X>\<C-O>"
                        endif
                elseif &dictionary != ''
                        return "\<C-K>"
                else
                        return "\<C-N>"
                endif
        endif
endfunction

inoremap <Tab> <C-R>=SuperCleverTab()<cr>


" Changing CaSe!
"
function! TwiddleCase(str)
        if a:str ==# toupper(a:str)
                let result = tolower(a:str)
        elseif a:str ==# tolower(a:str)
                let result = substitute(a:str,'\(\<\w\+\>\)', '\u\1', 'g')
        else
                let result = toupper(a:str)
        endif
        return result
endfunction
vnoremap <F5> ygv"=TwiddleCase(@")<CR>Pgv

function FunctionHeading()
  let s:line=line(".")
  call setline(s:line,"/*********************************************")
  call append(s:line,"* Description - ")
  call append(s:line+1,"* Author - RK")
  call append(s:line+2,"* Date - ".strftime("%b %d %Y"))
  call append(s:line+3,"* *******************************************/")
  unlet s:line
endfunction

imap <F4> <Esc>mz:execute FunctionHeading()<CR>`zjA
nmap <F4> mz:execute FunctionHeading()<CR>`zjA


" Help delete character if it is 'empty space'
" stolen from Vim manual
function! Eatchar()
  let c = nr2char(getchar())
  return (c =~ '\s') ? '' : c
endfunction

" Replace abbreviation if we're not in comment or other unwanted places
" stolen from Luc Hermitte's excellent http://hermitte.free.fr/vim/
function! MapNoContext(key, seq)
  let syn = synIDattr(synID(line('.'),col('.')-1,1),'name')
  if syn =~? 'comment\|string\|character\|doxygen'
    return a:key
  else
    exe 'return "' .
    \ substitute( a:seq, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) . '"'
  endif
endfunction

" Create abbreviation suitable for MapNoContext
function! Iab (ab, full)
  exe "iab <silent> <buffer> ".a:ab." <C-R>=MapNoContext('".
    \ a:ab."', '".escape (a:full.'<C-R>=Eatchar()<CR>', '<>\"').
    \"')<CR>"
endfunction

call Iab('#d', '#define ')
call Iab('#i', '#include <><Left>')
call Iab('#I', '#include ""<Left>')
call Iab('printf', 'printf("\n");<C-O>?\<CR>')
call Iab('if', 'if() {<CR>}<Left><C-O>?)<CR>')
call Iab('for', 'for(;;) {<CR>}<C-O>?;;<CR>')
call Iab('while', 'while() {<CR>}<C-O>?)<CR>')
call Iab('else', 'else {<CR>x;<CR>}<C-O>?x;<CR><Del><Del>')
call Iab('ifelse', 'if() {<CR>} else {<CR>}<C-O>?)<CR>')
call Iab('intmain', 'int main (int argc, char **argv)<CR>'.
 \ '{<CR>x;<CR>return 0;<CR>}<CR><C-O>?x;<CR><Del><Del>')

nmap _if ofprintf(0<C-d>stderr, "DEBUG: %s:%d - \n", __FILE__, __LINE__);<Esc>F\i
