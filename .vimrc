let mapleader=","
let g:mapleader=","

set nocompatible
syntax on
filetype plugin indent on

execute pathogen#infect()

set t_Co=256
set background=dark
color base16-default

" fast saving
nmap <leader>w :w!<cr>

set history=10000
set so=7
set enc=utf-8
set wildmenu

" ignore compiled files
set wildignore=*.o,*~,*.pyc
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set wildignore+=.git\*,.hg\*,.svn\*
set wildmode=list:full

" change working directory automatically
set autochdir

set ruler
set cmdheight=2
set guioptions-=T
set guioptions-=L
set guioptions-=r
if has("gui_running")
    set guifont=Consolas:h15
endif

" Show suspicious characters
set listchars=nbsp:¬,tab:>-,extends:»,precedes:«,trail:•

" Do syntax highlighting from the start
autocmd BufEnter * :syntax sync fromstart

set ignorecase
set smartcase
set hlsearch
set incsearch

" split edit vimrc
nnoremap <leader>evr <C-w><C-s><C-l>:e ~/.vimrc.after<CR>

set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set autoindent
set laststatus=2
set showmatch
set cursorline
set showcmd
set showtabline=2

" Store temporary files in a central spot
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

"Conque term
map <leader>zsh :ConqueTermSplit zsh<cr>


" make j and k work as expected for long lines
map j gj
map k gk

map <return> :nohlsearch<cr>
map <space> /
map <c-space> ?
map <leader>a :A<cr>

map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" arrow keys move visible lines
inoremap <Down> <C-R>=pumvisible() ? "\<lt>Down>" : "\<lt>C-O>gj"<CR>
inoremap <Up> <C-R>=pumvisible() ? "\<lt>Up>" : "\<lt>C-O>gk"<CR>
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk

" cycle through tabs browser style
map <leader><tab> :tabn<cr>

" CtrlP
let ctrlp_map = '<c-p>'
let ctrlp_cmd = 'CtrlP'
let ctrlp_working_path_mode = 'ra'

" NERDTree
map <leader>n :NERDTreeToggle<CR>

map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" command to enable soft-wrap
command! -nargs=* Wrap set wrap linebreak nolist

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" STATUS LINE
" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

" Return to last edit postition when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

" Remember info about open buffers on close
set viminfo^=%
set laststatus=2
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l

map 0 ^

func! DeleteTrailingWS()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>rn :call RenameFile()<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RemoveFancyCharacters COMMAND
" Remove smart quotes, etc.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RemoveFancyCharacters()
    let typo = {}
    let typo["“"] = '"'
    let typo["”"] = '"'
    let typo["‘"] = "'"
    let typo["’"] = "'"
    let typo["–"] = '--'
    let typo["—"] = '---'
    let typo["…"] = '...'
    :exe ":%s/".join(keys(typo), '\|').'/\=typo[submatch(0)]/ge'
endfunction
command! RemoveFancyCharacters :call RemoveFancyCharacters()


" -----------------------------------------------
"  latex compilation
"  ----------------------------------------------

autocmd BufEnter *.tex :color base16-tomorrow
autocmd BufWritePost *.tex silent !pdflatex %

" -----------------------------------------------
"  helper functions
"  ----------------------------------------------

function! HasPaste()
    if &paste
        return 'PASTE MODE'
    en
    return ''
endfunction
