call plug#begin('~/.vim/plugged')
Plug 'romainl/Apprentice'
Plug 'ctrlpvim/ctrlp.vim'
" Plug 'Valloric/YouCompleteMe', { 'for': ['c', 'c++', 'java'] }
Plug 'Raimondi/delimitMate' " Auto closing of brackets, etc.
Plug 'axelf4/vim-strip-trailing-whitespace'
Plug 'mattn/emmet-vim'
Plug 'junegunn/goyo.vim'

Plug 'rust-lang/rust.vim'
Plug 'pangloss/vim-javascript'
call plug#end()

set backspace=indent,eol,start " Allow backspace in insert mode
set tabstop=4 shiftwidth=0 noexpandtab " 4 columns wide tabs
set hidden " Hide buffers instead of unloading them
set wildmenu wildmode=longest:full,full " Enhance command-line completion
set incsearch ignorecase smartcase " Matches as you type, case-insensitive without capitals
set splitbelow splitright
set nohlsearch " Package maintainers keep meddling with the defaults

silent! colorscheme apprentice
let mapleader = "\<Space>"
nnoremap Y y$| " Make Y behave like other capitals
nnoremap <F5> :wall<Bar>make<CR>
" Write with sudo from within Vim
command! -bar W w !sudo tee % >/dev/null
" Repeat g; if the cursor is already at the previous change
nnoremap <expr> g; {l, c -> l[0]->get(l[1] - 1, {}) == #{lnum: c[1], col: c[2] - 1,
			\ coladd: c[3]}}(getchangelist(), getcurpos()) + 1 .. 'g;'
augroup vimrc
	autocmd!
augroup END

let ctrlp_working_path_mode = 0
let ctrlp_use_caching = 0
if executable("rg")
	set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
	let ctrlp_user_command = 'rg %s --files --color=never --glob ""'
endif

let delimitMate_expand_cr = 1
let delimitMate_balance_matchpairs = 1

let r_indent_align_args = 0 " No R function argument aligning
let g:rustfmt_autosave = 1
