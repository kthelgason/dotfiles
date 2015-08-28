set nocompatible
filetype off

" All vundle magic
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"
" " let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
"
" " The following are examples of different formats supported.
" " Keep Plugin commands between vundle#begin/end.
" " plugin on GitHub repo
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'majutsushi/tagbar'
Plugin 'bling/vim-airline'
Plugin 'guns/vim-clojure-static'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'tpope/vim-fireplace'
Plugin 'jimenezrick/vimerl'
Plugin 'junegunn/goyo.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'raichoo/purescript-vim'
Plugin 'digitaltoad/vim-jade'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'fatih/vim-go'

" End Vundle magic
call vundle#end()            " required

let mapleader=","
let g:mapleader=","

filetype plugin indent on

set t_Co=256
set background=dark
color jellybeans
syntax on

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
set cmdheight=1
set guioptions-=T
set guioptions-=L
set guioptions-=r
if has("gui_running")
    set guifont=Anonymice\ Powerline:h16
    set fuoptions=maxvert,maxhorz
    let g:airline_powerline_fonts=1
endif
set ttyfast

" Show suspicious characters
set listchars=nbsp:¬,tab:>-,extends:»,precedes:«,trail:•
set list

" Do syntax highlighting from the start
autocmd BufEnter * :syntax sync fromstart

set ignorecase
set smartcase
set hlsearch
set incsearch

" Apparently this magic makes vim use os x clipboard
set clipboard=unnamed

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

" Tagbar plugin settings
nmap <f8> :TagbarToggle<cr>
map <leader>T :TagbarOpenAutoClose<cr>

" CtrlP
let ctrlp_map = '<c-p>'
let ctrlp_cmd = 'CtrlP'
let ctrlp_working_path_mode = 'ra'
let g:ctrlp_use_caching = 0
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
else
    let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
    let g:ctrlp_prompt_mappings = {
        \ 'AcceptSelection("e")': ['<space>', '<cr>', '<2-LeftMouse>'],
    \ }
endif

" NERDTree
map <leader>n :NERDTreeToggle<CR>
let NERDTreeIgnore=['\~$', '\.pyc$', '\.pyo$', '\.class$', 'pip-log\.txt$', '\.o$', '\.dSYM$']

" cycle through tabs browser style
map <leader><tab> :tabn<cr>

" cd to current dir
map <leader>cd :cd %:p:h<cr>

" split edit vimrc
nnoremap <leader>evr <C-w><C-s><C-l>:e ~/.vimrc<CR>

" quick open new files
nnoremap <leader>o :CtrlP<CR>

" Copy & paste to system clipboard
vmap <leader>y "+y
vmap <leader>d "+d
vmap <leader>p "+p
vmap <leader>P "+P
nmap <leader>p "+p
nmap <leader>P "+P

" jump to end on paste
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" sudo write
cmap W! w !sudo tee % >/dev/null

" Stop that stupid window from popping up
map q: :q

map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" enforce 80 char row limit
"highlight OverLength ctermbg=darkred ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

"Airline settings
let g:airline#extensions#ctrlp#color_template = 'replace'
let g:airline_section_y = airline#section#create_right(['%{printf("%s%s",&fenc,&ff!="unix"?":".&ff:"")}'])
let g:airline_section_z = airline#section#create_right(['%3l:%2c'])

" Syntastic settings
let g:syntastic_auto_loc_list=0
let g:syntastic_enable_signs=1
let g:syntastic_check_on_wq=1
let g:syntastic_python_checkers = ['python']

" Return to last edit postition when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

" Remember info about open buffers on close
set viminfo^=%

set pastetoggle=<F2>
map 0 ^

autocmd FileType ruby,haml,jade,eruby,yaml,html,sass,cucumber set ai sw=2 sts=2 et
autocmd FileType javascript,coffeescript set ai sw=2 sts=2 et
autocmd FileType python set sw=4 sts=4 et

map <leader>b :CtrlPBuffer<cr>

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
"  Clojure
"  ----------------------------------------------
au VimEnter *.clj RainbowParenthesesActivate
au BufEnter *.clj :RainbowParenthesesLoadRound
au BufEnter *.clj :RainbowParenthesesLoadSquare
au BufEnter *.clj :RainbowParenthesesLoadBraces

" -----------------------------------------------
"  markdown
"  ----------------------------------------------
let g:vim_markdown_folding_disabled=1

" -----------------------------------------------
"  helper functions
"  ----------------------------------------------
function! HasPaste()
    if &paste
        return 'PASTE MODE'
    en
    return ''
endfunction

" vp doesn't replace paste buffer
function! RestoreRegister()
    let @" = s:restore_reg
    return ''
endfunction
function! s:Repl()
    let s:restore_reg = @"
    return "p@=RestoreRegister()\<cr>"
endfunction
vmap <silent> <expr> p <sid>Repl()

