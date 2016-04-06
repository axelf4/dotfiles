set nocompatible " be IMproved
filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'xoria256.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'
Plugin 'altercation/vim-colors-solarized'
" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

set backspace=indent,eol,start " Allow backspace in insert mode
set shiftwidth=4 tabstop=4 noexpandtab " Tabs are 4 columns wide
set number " Enable line numbers
set hidden " Hide buffers instead of closing them
set wildmenu " Enhance command-line completion
set incsearch ignorecase smartcase " Matches as you type, case-sensitive if any caps used
" set clipboard=unnamed " Use the OS clipboard by default (on versions compiled with `+clipboard`)
set autoread " Automatically reload files changed externally
set encoding=utf-8
syntax on " Enable syntax highlighting
colorscheme xoria256 " Use the Solarized Dark theme
let mapleader = "\<Space>"

autocmd BufWritePre * :%s/\s\+$//e " Automatically remove trailing whitespace
" Write as su
command W w !sudo tee % > /dev/null

if executable("ag")
    set grepprg=ag\ --nogroup\ --nocolor
	" let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
	" let g:ctrlp_user_command = 'ag -l --nocolor -g "" --path-to-agignore=~/.agignore %s'
	let g:ctrlp_use_caching = 0
endif

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
" let g:UltiSnipsJumpBackwardTrigger="<s-c-j>"
