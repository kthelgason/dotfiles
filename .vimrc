set background=dark
color tomorrow-night-bright

" fast saving
nmap <leader>w :w!<cr>

set so=7
set enc=utf-8
set wildmenu

" ignore compiled files
set wildignore=*.o,*~,*.pyc
set wildignore+=.git\*,.hg\*,.svn\*
set wildmode=list:full

" change working directory automatically
set autochdir

set ruler
set gdefault
set cmdheight=2
set guioptions-=T
set guioptions-=L
set guioptions-=r
if has("gui_running")
    set cursorline
    set guifont=Consolas:h15
endif
set ttyfast
set hid
set autoread

" Show suspicious characters
set listchars=nbsp:¬,tab:>-,extends:»,precedes:«,trail:•

" Do syntax highlighting from the start
autocmd BufEnter * :syntax sync fromstart

set ignorecase
set smartcase
set hlsearch
set incsearch
" fantastic setting
set magic

" split edit vimrc
nnoremap <leader>evr <C-w><C-s><C-l>:e ~/.vimrc.after<CR>

set encoding=utf8
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4

set ai
set si
set wrap

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

" cycle through tabs browser style
map <leader><tab> :tabn<cr>


map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" command to enable soft-wrap
command! -nargs=* Wrap set wrap linebreak nolist


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
