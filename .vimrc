set nocompatible " be iMproved
set backspace=indent,eol,start " Allow backspace in insert mode
set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab " Tabs are 4 columns wide
set wildmenu " Enhance command-line completion
set clipboard=unnamed " Use the OS clipboard by default (on versions compiled with `+clipboard`)
set browsedir=buffer " File, Open dialog defaults to current file's directory
set number " Enable line numbers
set autoread " Reload files automatically

syntax on " Enable syntax highlighting
set background=dark
colorscheme solarized " Use the Solarized Dark theme

autocmd BufWritePre * :%s/\s\+$//e " Automatically remove trailing whitespace

inoremap jk <ESC> " Fast exit from Insert Mode
nnoremap <CR> G " Jump to line number with 123<Enter> instead of 123G/gg

cnoremap sudow w !sudo tee % >/dev/null

filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'Valloric/YouCompleteMe'

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on
