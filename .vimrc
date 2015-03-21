set nocompatible " be iMproved
set backspace=indent,eol,start " Allow backspace in insert mode
set tabstop=4 " Have tabs as wide as four spaces
syntax on " Enable syntax highlighting
set wildmenu " Enhance command-line completion
set clipboard=unnamed " Use the OS clipboard by default (on versions compiled with `+clipboard`)
set browsedir=buffer " File, Open dialog defaults to current file's directory

colorscheme corporation

" Automatically remove trailing whitespace
autocmd BufWritePre * :%s/\s\+$//e

" Fast exit from Insert Mode
inoremap jk <ESC>
" Jump to line number with 123<Enter> instead of 123G/gg
nnoremap <CR> G

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
