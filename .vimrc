set nocompatible " be IMproved
set backspace=indent,eol,start " Allow backspace in insert mode
set shiftwidth=4 tabstop=4 noexpandtab " Tabs are 4 columns wide
set wildmenu " Enhance command-line completion
set number " Enable line numbers
set hidden " Hide buffers instead of closing them
set incsearch ignorecase smartcase " Matches as you type, case-sensitive if any caps used
set encoding=utf-8
" set clipboard=unnamed " Use the OS clipboard by default (on versions compiled with `+clipboard`)
set autoread " Automatically reload files changed externally

filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'xoria256.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'Valloric/YouCompleteMe'
" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

syntax on " Enable syntax highlighting
colorscheme xoria256 " Use the Solarized Dark theme

autocmd BufWritePre * :%s/\s\+$//e " Automatically remove trailing whitespace
cnoremap w!! w !sudo tee % " Sudo to write

if executable("ag")
    set grepprg=ag\ --nogroup\ --nocolor
	" let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
	let g:ctrlp_user_command = 'ag -l --nocolor -g "" --path-to-agignore=~/.agignore %s'
	let g:ctrlp_use_caching = 0
endif
