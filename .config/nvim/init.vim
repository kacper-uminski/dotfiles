" Vim-Plug

call plug#begin('~/.config/nvim/plugged')

Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
Plug 'itchyny/lightline.vim'

call plug#end()


" Lightline

set laststatus=2
set noshowmode
let g:lightline = { 'colorscheme': 'challenger_deep'}

if !has('gui_running')
  set t_Co=256
endif


" Color Scheme

colorscheme challenger_deep
if has('nvim') || has('termguicolors')
  set termguicolors
endif


" Syntax Highlighting

syntax on


" Line Numbers

set number
set relativenumber

