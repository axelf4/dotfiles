" Vim indent file
" Language: MATLAB
" Maintainer: Axel Forsman <axelsfor@gmail.com>
" License: MIT

" Only load if no other indent file is loaded
if exists('b:did_indent') | finish | endif
let b:did_indent = 1

setlocal indentexpr=GetMatlabIndent()
setlocal indentkeys=!,o,O,=end,0=case,0=else,0=elseif,0=otherwise,0=catch

" Only define the function once
if exists("*GetMatlabIndent") | finish | endif

let s:open_pattern = '\C\<\%(function\|for\|if\|parfor\|spmd\|switch\|try\|while\|classdef\|properties\|methods\|events\|enumeration\)\>'
let s:middle_pattern = '\C^\s*\<\%(case\|catch\|else\|elseif\|otherwise\)\>'
let s:close_pattern = '\C\<\%(end\)\>'
" Expression used to check whether we should skip a match with searchpair()
let s:skip_expr = 'synIDattr(synID(line("."), col("."), 1), "name") =~# ''matlabComment\|matlabMultilineComment\|matlabString'''

" Returns whether the specified line continues on the next line.
function! s:IsLineContinuation(lnum)
	let [l, c] = [getline(a:lnum), -3]
	while 1
		let c = match(l, '\.\{3}', c + 3)
		if c == -1 | return 0
		elseif synIDattr(synID(a:lnum, c, 1), "name") !~# 'matlabComment\|matlabMultilineComment\|matlabString' | return 1 | endif
	endwhile
endfunction

function! GetMatlabIndent()
	let prev_line = prevnonblank(v:lnum - 1)

	" Count how many blocks the previous line opens
	call cursor(v:lnum, 1)
	let num_prev_opens = searchpair(s:open_pattern, s:middle_pattern, s:close_pattern,
				\ 'mrb', s:skip_expr, prev_line)

	" Count how many blocks the current line closes
	call cursor(prev_line, col([prev_line, '$']))
	let num_cur_closes = searchpair(s:open_pattern, s:middle_pattern, s:close_pattern,
				\ 'mr', s:skip_expr, v:lnum)

	let i = num_prev_opens - num_cur_closes
				\ + s:IsLineContinuation(v:lnum - 1) - s:IsLineContinuation(prev_line - 1)
	return indent(prev_line) + &shiftwidth * i
endfunction
