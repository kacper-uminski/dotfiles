" Vim-Plug
call plug#begin('~/.vim/plugged')

Plug 'bfrg/vim-cpp-modern'
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
Plug 'itchyny/lightline.vim'

call plug#end()

" Lightline
if !has('gui_running')
  set t_Co=256
endif
set laststatus=2
set noshowmode
let g:lightline = { 'colorscheme': 'challenger_deep'}

" Color Scheme
colorscheme challenger_deep

" Disable Bars
set guioptions-=m  "menu bar
set guioptions-=T  "toolbar
set guioptions-=r  "scrollbar

" Syntax Highlighting
syntax on

" Line Numbers
set number
