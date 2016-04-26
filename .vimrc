call plug#begin('~/.vim/plugged')
Plug 'xoria256.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'Valloric/YouCompleteMe'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'altercation/vim-colors-solarized'
call plug#end()

set number " Enable line numbers
set backspace=indent,eol,start " Allow backspace in insert mode
set shiftwidth=4 tabstop=4 noexpandtab " Tabs are 4 columns wide
set hidden " Hide buffers instead of closing them
set wildmenu " Enhance command-line completion
set incsearch ignorecase smartcase " Matches as you type, case-sensitive if any caps used
" set clipboard=unnamed " Use the OS clipboard by default (on versions compiled with `+clipboard`)

let mapleader = "\<Space>"
colorscheme xoria256

" Write as su
command W w !sudo tee % > /dev/null
autocmd BufWritePre * :%s/\s\+$//e " Automatically remove trailing whitespace

if executable("ag")
    set grepprg=ag\ --nogroup\ --nocolor
	let g:ctrlp_use_caching = 0
endif
