if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

let s:save_cpo = &cpo
set cpo&vim

if exists("loaded_matchit")
	let s:conditionalEnd = '\%(([^()]*\)\@!\<end\>\%([^()]*)\)\@!'
	let b:match_words=
				\ '\<\%(if\|switch\|for\|while\)\>:\<\%(elseif\|case\|break\|continue\|else\|otherwise\)\>:'.s:conditionalEnd.','.
				\ '\<function\>:\<return\>:\<endfunction\>'
endif

setlocal suffixesadd=.m
setlocal suffixes+=.asv

setlocal comments=:%
setlocal commentstring=%%s
setlocal formatoptions+=ronl

setlocal include=^\\s*import
setlocal includeexpr=substitute(v:fname,'\\(\\w\\+\\)\\.','+\\1/','g')

setlocal tagfunc=matlab#Tagfunc

if exists('&omnifunc')
	setlocal omnifunc=matlab#CompleteMatlab
endif

" Jumps to the next/previous code section.
function! s:NextSection(mode, backwards)
	if a:mode ==# 'v'
		normal! gv
	endif

	let foldenable = &foldenable
	set nofoldenable
	let i = v:count1
	while i > 0
		call search('^\s*\zs%%\|\%^', 'sW' . (a:backwards ? 'b' : ''))
		let i -= 1
	endwhile
	let &foldenable = foldenable
endfunction

nnoremap <script> <silent> <buffer> ]] :call <SID>NextSection('n', 0)<CR>
nnoremap <script> <silent> <buffer> [[ :call <SID>NextSection('n', 1)<CR>
xnoremap <script> <silent> <buffer> ]] :call <SID>NextSection('v', 0)<CR>
xnoremap <script> <silent> <buffer> [[ :call <SID>NextSection('v', 1)<CR>
onoremap <script> <silent> <buffer> ]] :call <SID>NextSection('o', 0)<CR>
onoremap <script> <silent> <buffer> [[ :call <SID>NextSection('o', 1)<CR>

let b:undo_ftplugin = "setlocal suffixesadd< suffixes< comments< commentstring< formatoptions< omnifunc<
			\ | nunmap <buffer> [[
			\ | nunmap <buffer> ]]
			\ | xunmap <buffer> [[
			\ | xunmap <buffer> ]]
			\ | ounmap <buffer> [[
			\ | ounmap <buffer> ]]
			\ | unlet! b:match_words"

let &cpo = s:save_cpo
