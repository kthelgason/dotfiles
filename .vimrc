set nocompatible

let mapleader=","
let g:mapleader=","

syntax on
filetype plugin indent on

execute pathogen#infect()

set t_Co=256
set background=dark
color ir_black

" fast saving
nmap <leader>w :w!<cr>

set history=10000
set so=7
set enc=utf-8
set wildmenu

" ignore compiled files
set wildignore=*.o,*~,*.pyc
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set wildignore+=.git/*,.hg/*,.svn/*
set wildmode=list:full

set ruler
set cmdheight=2
set guioptions-=T
set guioptions-=L
set guioptions-=r
if has("gui_running")
    set guifont=Anonymous\ Pro:h13
    color base16-default
endif
set ttyfast

" Show suspicious characters
set listchars=nbsp:¬,tab:>-,extends:»,precedes:«,trail:•

" Do syntax highlighting from the start
autocmd BufEnter * :syntax sync fromstart

set ignorecase
set smartcase
set hlsearch
set incsearch

" split edit vimrc
nnoremap <leader>evr <C-w><C-s><C-l>:e ~/.vimrc<CR>

set expandtab
set smarttab
set number
set shiftwidth=4
set tabstop=4
set autoindent
set laststatus=2
set showmatch
set cursorline
set showcmd
set showtabline=2
set scrolloff=5
set noerrorbells
set backspace=indent,eol,start
set autoread

" Store temporary files in a central spot
set backup
set backupdir=~/.vim/sessions
set directory=~/.vim/sessions
" make j and k work as expected for long lines
map j gj
map k gk

map <return> :nohlsearch<cr>
map <space> /
map <c-space> ?
map <leader>a :A<cr>
inoremap jj <esc>

map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l
"
" Tagbar plugin settings
map <leader>tb :TagbarToggle<cr>
map <leader>T :TagbarOpenAutoClose<cr>

" cycle through tabs browser style
map <leader><tab> :tabn<cr>

" cd to current dir
map <leader>cd :cd %:p:h<cr>


" sudo write
cmap W! w !sudo tee % >/dev/null

" CtrlP
let ctrlp_map = '<c-p>'
let ctrlp_cmd = 'CtrlP'
let ctrlp_working_path_mode = 'ra'

" NERDTree
map <leader>n :NERDTreeToggle<CR>
let NERDTreeIgnore=['\~$', '\.pyc$', '\.pyo$', '\.class$', 'pip-log\.txt$', '\.o$', '\.dSYM$']

map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" command to enable soft-wrap
command! -nargs=* Wrap set wrap linebreak nolist

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" STATUS LINE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set statusline=\ %{HasPaste()}[%n%M]\ %f\ [%R%H%W%Y]\ %{fugitive#statusline()}\ CWD:%<%{getcwd()}\ loc:\ %l/%L,%v
set statusline+=%{SyntasticStatuslineFlag()}

" Syntastic settings
let g:syntastic_auto_loc_list=1
let g:syntastic_loc_list_height=5
let g:syntastic_python_checkers = ['pep8']
map <leader>b :CtrlPBuffer<cr>

" Return to last edit postition when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

" Remember info about open buffers on close
set viminfo^=%

set pastetoggle=<F2>
map 0 ^

autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et
autocmd FileType python set sw=4 sts=4 et
map <leader>p <esc>:! pyflakes %<cr>

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
"  markdown 
"  ----------------------------------------------
let g:vim_markdown_folding_disabled=1

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

