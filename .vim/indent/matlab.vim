" Vim indent file
" Language: MATLAB
" Maintainer: Axel Forsman <axelsfor@gmail.com>
" License: MIT

" Only load if no other indent file is loaded
if exists('b:did_indent') | finish | endif
let b:did_indent = 1

setlocal indentexpr=GetMatlabIndent()
setlocal indentkeys=!,o,O,=end,e,0=elsei,0=elseif,0=case,0=otherwise,0=catch

" Only define the function once
if exists("*GetMatlabIndent") | finish | endif

let s:middle_pat = 'else\|elseif\|case\|otherwise\|catch'
let s:pair_pat = '\C\(\<\%(function\|for\|if\|parfor\|spmd\|switch\|try\|while\|classdef\|properties\|methods\|events\|enumeration\|\%(^\s*\)\@<=\%(' . s:middle_pat . '\)\)\>\|\[\|{\)\|\(\%(\S.*\)\@<=\<end\>\|\]\|}\)'
let s:dedent_pat = '\C^\s*\%(end\|' . s:middle_pat . '\)\>'

" Returns whether a comment or string envelops the specified column.
function! s:IsCommentOrString(lnum, col)
	return synIDattr(synID(a:lnum, a:col, 1), "name") =~# 'matlabComment\|matlabMultilineComment\|matlabString'
endfunction

" Returns whether the specified line continues on the next line.
function! s:IsLineContinuation(lnum)
	let [l, c] = [getline(a:lnum), -3]
	while 1
		let c = match(l, '\.\{3}', c + 3)
		if c == -1 | return 0
		elseif !s:IsCommentOrString(a:lnum, c) | return 1 | endif
	endwhile
endfunction

function! GetMatlabIndent()
	let prev_lnum = prevnonblank(v:lnum - 1)

	let i = 0
	" Count how many blocks the previous line opens/closes
	if prev_lnum != 0
		let prev_line = getline(prev_lnum)
		call cursor(prev_lnum, 1)
		while 1
			let [lnum, c, submatch] = searchpos(s:pair_pat, 'cpz', prev_lnum)
			if submatch == 0 | break | endif
			if !s:IsCommentOrString(prev_lnum, c)
						\ && prev_line[c - 3:] !~# 'end[^(]*)' " Array indexing (no line continuations!)
				let i += submatch == 2 ? 1 : -1
			endif
			if c == col('$') - 1 | break | endif
			call cursor(0, c + 1)
		endwhile
	endif

	" Dedent 'end', etc. at start of line
	call cursor(v:lnum, 1)
	let i -= search(s:dedent_pat, 'cz', v:lnum) != 0 && !s:IsCommentOrString(v:lnum, col('.'))

	" Line continuations indent once per statement
	let i += s:IsLineContinuation(v:lnum - 1) - s:IsLineContinuation(prev_lnum - 1)

	return indent(prev_lnum) + &shiftwidth * i
endfunction
