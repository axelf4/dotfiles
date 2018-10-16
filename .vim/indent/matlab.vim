" Vim indent file
" Language: MATLAB
" Author: Axel Forsman <axelsfor@gmail.com>
" License: MIT

" Only load if no other indent file is loaded
if exists('b:did_indent') | finish | endif
let b:did_indent = 1

setlocal indentexpr=GetMatlabIndent()
setlocal indentkeys=!,o,O,0=end,e,0=elsei,0=elseif,0=case,0=otherwise,0=catch,0=function

" The value of the Function indenting format in
" MATLAB Editor/Debugger Language Preferences.
" The possible values are 0 for Classic, 1 for Indent nested functions
" and 2 for Indent all functions (default).
let b:MATLAB_function_indent = get(g:, 'MATLAB_function_indent', 2)

let s:open_pat = 'for\|if\|parfor\|spmd\|switch\|try\|while\|classdef\|properties\|methods\|events\|enumeration'
let s:middle_pat = 'else\|elseif\|case\|otherwise\|catch'
let b:pair_pat = '\C\(\<\%('
			\ . (b:MATLAB_function_indent == 1 ? '\s\@<=' : '')
			\ . (b:MATLAB_function_indent >= 1 ? 'function\|' : '')
			\ . s:open_pat . '\|\%(^\s*\)\@<=\%(' . s:middle_pat . '\)\)\>\|\[\|{\)'
			\ . '\|\(\%(\S.*\)\@<=\<end\>\|\]\|}\)'

" Only define the function once
if exists("*GetMatlabIndent") | finish | endif

let s:dedent_pat = '\C^\s*\<\%(end\|else\|elseif\|catch\|otherwise\|\(case\|function\)\)\>'
let s:start_pat = '\C\<\%(function\|' . s:open_pat . '\)\>'
let s:search_flags = 'cp' . (has('patch-7.4.984') ? 'z' : '')

" Returns whether a comment or string envelops the specified column.
function! s:IsCommentOrString(lnum, col)
	return synIDattr(synID(a:lnum, a:col, 1), "name") =~# 'matlabComment\|matlabMultilineComment\|matlabString'
endfunction

" Returns whether the specified line continues on the next line.
function! s:IsLineContinuation(lnum)
	let l = getline(a:lnum) | let c = -3
	while 1
		let c = match(l, '\.\{3}', c + 3)
		if c == -1 | return 0
		elseif !s:IsCommentOrString(a:lnum, c) | return 1 | endif
	endwhile
endfunction

function! s:GetOpenCloseCount(lnum, ...)
	let endcol = a:0 >= 1 ? a:1 : 0
	let i = 0 | let line = getline(a:lnum)
	call cursor(a:lnum, 1)
	while 1
		let [lnum, c, submatch] = searchpos(b:pair_pat, s:search_flags, a:lnum)
		if !submatch || endcol && c >= endcol | break | endif
		if !s:IsCommentOrString(lnum, c)
					\ && line[c - 3:] !~# 'end[^(]*)' " Array indexing heuristic
			let i += submatch == 2 ? 1 : -1
		endif
		if c == col('$') - 1 | break | endif
		call cursor(0, c + 1)
	endwhile
	return i
endfunction

function! GetMatlabIndent()
	" Align end, etc. with start of block
	call cursor(v:lnum, 1)
	let submatch = search(s:dedent_pat, s:search_flags, v:lnum)
	if submatch && !s:IsCommentOrString(v:lnum, col('.'))
		let [lnum, col] = searchpairpos(s:start_pat, '',  '\C\<end\>', 'bW', 's:IsCommentOrString(line("."), col("."))')
		return lnum ? indent(lnum) + shiftwidth() * (s:GetOpenCloseCount(lnum, col) + submatch == 2) : 0
	endif

	let prev_lnum = prevnonblank(v:lnum - 1)
	" Count how many blocks the previous line opens/closes
	let i = prev_lnum ? s:GetOpenCloseCount(prev_lnum) : 0

	" Line continuations indent once per statement
	let i += s:IsLineContinuation(v:lnum - 1) - s:IsLineContinuation(v:lnum - 2)

	return indent(prev_lnum) + shiftwidth() * i
endfunction
