call plug#begin('~/.vim/plugged')
Plug 'romainl/Apprentice'
Plug 'ctrlpvim/ctrlp.vim'
" Plug 'Valloric/YouCompleteMe', { 'for': ['c', 'c++', 'java'] }

Plug 'rust-lang/rust.vim'
call plug#end()

set number " Enable line numbers
set backspace=indent,eol,start " Allow backspace in insert mode
set shiftwidth=4 tabstop=4 noexpandtab " Tabs are 4 columns wide
set hidden " Hide buffers instead of closing them
set wildmenu " Enhance command-line completion
set incsearch ignorecase smartcase " Matches as you type, case-sensitive if capitals are used
" set clipboard=unnamed " Use the OS clipboard by default (on versions compiled with `+clipboard`)

let mapleader = "\<Space>"
silent! colorscheme apprentice

nnoremap <F5> :wall<Bar>make<CR>
" Write as su
command W w !sudo tee % > /dev/null
autocmd BufWritePre * :%s/\s\+$//e " Automatically remove trailing whitespace

let g:ctrlp_working_path_mode = 0
let g:ctrlp_use_caching = 0
if executable("rg")
	set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
	let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
endif

let g:rustfmt_autosave = 1
