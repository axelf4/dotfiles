call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tmsvg/pear-tree' " Auto closing of brackets, etc.
Plug 'editorconfig/editorconfig-vim'
Plug 'axelf4/vim-strip-trailing-whitespace'
Plug 'mattn/emmet-vim'
Plug 'junegunn/goyo.vim'
Plug 'romainl/Apprentice'

Plug 'rust-lang/rust.vim'
Plug 'axelf4/vim-haskell'
Plug 'pangloss/vim-javascript'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'LnL7/vim-nix'
call plug#end()

set backspace=indent,eol,start " Allow backspace in insert mode
set tabstop=4 shiftwidth=0 noexpandtab " 4 columns wide tabs
set hidden " Hide buffers instead of unloading them
set wildmenu wildmode=longest:full,full " Enhance command-line completion
set incsearch ignorecase smartcase " Matches as you type, case-insensitive without capitals
set nojoinspaces " No double spaces between sentences
set formatoptions+=j " Remove comment leaders when joining lines
set splitbelow splitright
set cpoptions+=M " Fix Haskell lambda with tuple pattern messing with "%"

silent! colorscheme apprentice
let mapleader = "\<Space>"
nnoremap Y y$| " Make Y behave like other capitals
" Repeat g; if the cursor is already at the previous change
nnoremap <expr> g; {l, c -> l[0]->get(l[1] - 1, {}) == #{lnum: c[1], col: c[2] - 1,
			\ coladd: c[3]}}(getchangelist(), getcurpos()) + 1 .. 'g;'
nnoremap <F9> <Cmd>wall<Bar>make<CR>
noremap <leader>w <C-w>
" Write with sudo from within Vim
command! -bar W w !sudo tee % >/dev/null
augroup vimrc
	autocmd!
augroup END

let ctrlp_working_path_mode = 0
let ctrlp_use_caching = 0
if executable('rg')
	set grepprg=rg\ --vimgrep\ --smart-case
	let ctrlp_user_command = 'rg --color=never --files %s'
endif

let [pear_tree_smart_openers, pear_tree_smart_closers, pear_tree_smart_backspace] = [1, 1, 1]

let r_indent_align_args = 0 " No R function argument aligning
let rustfmt_autosave = 1
