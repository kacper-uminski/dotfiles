"Vim-Plug
call plug#begin('~/.config/nvim/plugged')
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
Plug 'itchyny/lightline.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neovimhaskell/haskell-vim'
call plug#end()


"Lightline
set laststatus=2
set noshowmode
let g:lightline = { 'colorscheme': 'challenger_deep'}
if !has('gui_running')
	set t_Co=256
endif

"Color Scheme
colorscheme challenger_deep
if has('nvim') || has('termguicolors')
	set termguicolors
endif
"Also add this to the theme: hi Normal guibg=NONE ctermbg=NONE

"Keymappings
tnoremap <Esc> <C-\><C-n>

"Coc keys
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"Syntax Highlighting
syntax on

"Haskell Syntax
let g:haskell_enable_quantification =	1	" to enable highlighting of `forall`
let g:haskell_enable_recursivedo =		1	" to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax =		1	" to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms =	1	" to enable highlighting of `pattern`
let g:haskell_enable_typeroles =		1	" to enable highlighting of type roles
let g:haskell_enable_static_pointers =	1	" to enable highlighting of `static`
let g:haskell_backpack =				1	" to enable highlighting of backpack keywords

"Line Numbers
set number
set relativenumber

"Whitespace
set autoindent
set noexpandtab
set shiftwidth=4
set tabstop=4
set listchars=space:_,tab:>~ list
autocmd Filetype haskell setlocal expandtab
autocmd Filetype python setlocal noexpandtab shiftwidth=4 tabstop=4
