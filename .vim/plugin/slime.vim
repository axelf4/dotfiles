" Vim plugin for sending text to other windows
scriptversion 4

const s:tmux_reset_seq = 'q C-u ' " Key sequence for leaving copy-mode and clearing prompt

" Operator that sends selection to other window.
function! s:SlimeOperator(...) abort
	if !a:0 | let &operatorfunc = expand('<sfile>')[9:] | return 'g@' | endif

	let type = a:1
	let [sel_save, reg_save] = [&selection, @@]
	try
		let &selection = "inclusive"
		silent execute 'normal! `[' .. (type ==# 'line' ? 'V' : type ==# 'char' ? 'v' : "\<C-V>") .. '`]y'
		let text = @@
	finally
		let [&selection, @@] = [sel_save, reg_save]
	endtry

	let text = text->split("\n")->filter({i, v -> !empty(v)})->join("\n") .. "\n"

	function! WindowCb(target) abort closure
		let b:slime_target = a:target
		call s:TmuxSend(a:target, text)
	endfunction
	if exists('b:slime_target')
		call WindowCb(b:slime_target)
	else
		call PromptWindow(funcref('WindowCb'))
	endif
endfunction

noremap <unique> <silent> <expr> <Plug>(SlimeSend) <SID>SlimeOperator()

if !hasmapto('<Plug>(SlimeSend)')
	nmap <Leader>s <Plug>(SlimeSend)
	xmap <Leader>s <Plug>(SlimeSend)
	nmap <Leader>ss <Plug>(SlimeSend)_
endif

" Prompts the user for a target window.
"
" Here `window` is using the Vim terminology.
" After this function returns, in the absence of errors the current
" buffer/window is guaranteed to be the prompt.
function! PromptWindow(Cb) abort
	let [head, windowLabels] = s:TmuxWinlayout()

	" Recursive procedure for drawing cells
	function! DrawCell(node, x, y, w, h) abort closure
		let type = a:node[0]
		if type ==# 'leaf'
			let text = windowLabels[a:node[1]][:a:w]
			execute 'normal! ' .. (a:y + a:h / 2) .. 'G' .. (a:x + (a:w - strdisplaywidth(text)) / 2) .. '|R' .. text .. "\<Esc>"
		else
			let nchildren = len(a:node[1]) | let l = type ==# 'row' ? a:w : a:h
			let percell = (l - (nchildren - 1)) / nchildren | let rem = (l - (nchildren - 1)) % nchildren
			let [cx, cy] = [a:x, a:y]
			for i in range(nchildren)
				let [cw, ch] = type ==# 'row' ? [percell + (i < rem), a:h] : [a:w, percell + (i < rem)]
				call DrawCell(a:node[1][i], cx, cy, cw, ch)
				let [cx, cy] += type ==# 'row' ? [cw, 0] : [0, ch]
				if i != nchildren - 1
					execute 'normal! ' .. cy .. 'G' .. cx .. "|\<C-V>" .. (type ==# 'row' ? (ch - 1) .. 'jr║' : (cw - 1) .. 'lr═')
				endif
				let [cx, cy] += type ==# 'row' ? [1, 0] : [0, 1]
			endfor
		endif
	endfunction
	silent keepalt new __Window_Select__ " Create new window
	setlocal buftype=nofile bufhidden=hide noswapfile nobuflisted nonumber nolist modifiable
	let b:Cb = a:Cb " Store callback in buffer
	let save_virtualedit = &virtualedit | set virtualedit=all
	silent normal! ggdGg$
	let [w, h] = [virtcol('.'), winheight(0)]
	call append(0, range(h - 1)->map('""')) " Fill buffer with lines
	silent call DrawCell(head, 1, 1, w, h)
	let &virtualedit = save_virtualedit
	setlocal nomodifiable
	silent execute "normal! gg/\\d\\+\<CR>"

	function! s:WindowSelectEnter() abort
		let word = expand('<cword>') " Get word under cursor
		if word =~# '\D' | return | endif
		let Cb = b:Cb
		quit
		call Cb('%' .. word)
	endfunction
	nnoremap <script> <buffer> <nowait> <silent> <CR> :call <SID>WindowSelectEnter()<CR>
	nnoremap <script> <buffer> <nowait> <silent> n /\d\+<CR>
	nnoremap <script> <buffer> <nowait> <silent> p ?\d\+<CR>

	augroup slimeprompt
		autocmd! * <buffer>
		autocmd WinLeave <buffer> quit
	augroup END
endfunction

" Parse tmux window layout into format compatible with `winlayout()`.
function! s:TmuxWinlayout() abort
	let layout = system('tmux display-message -p "#{window_layout}"')[5:-2] " Trim checksum & NL
	let stack = []
	while layout !~# '^\s*$' " While layout desc is not empty
		" Parse layout cell: window ptr and split type (see layout-custom.c)
		let [str, wp, sep, layout; rest] = matchlist(layout, '^\d\+x\d\+,\d\+,\d\+\%(,\(\d*\)x\@!\)\?\([{[\_$]\|[,}\]]*\)\(.*\)$')
		let type = sep ==# '[' ? 'col' : sep ==# '{' ? 'row' : 'leaf'
		let node = [type, type ==# 'leaf' ? wp : []]
		if empty(stack) | let head = node | else | call add(stack[-1][1], node) | endif " Add to parent
		if type !=# 'leaf' | call add(stack, node) | endif " Push to parent stack
		for sign in split(sep, '\zs')
			if sign ==# '}' || sign ==# ']' | unlet stack[-1] | endif
		endfor
	endwhile

	" Get names of commands running in panes
	let panes = split(system('tmux list-panes -F "#{pane_id}/#{pane_current_command}"'), "\n")
	let windowLabels = {}
	for pane in panes
		let [_, pane_id, current_command; rest] = matchlist(pane, '^%\?\([^/]\+\)/\(.*\)$')
		let windowLabels[pane_id] = pane_id .. ' ' .. current_command
	endfor

	return [head, windowLabels]
endfunction

" Sends the specified text to the target tmux window.
function! s:TmuxSend(target, text) abort
	let target = system('tmux display-message -p -t ' .. a:target .. ' "#{pane_id}"')[:-2]
	if target =~# '^can''t find pane '
		echo 'Failed to find target'
		return
	endif

	call system('tmux send-keys -t ' .. target .. ' ' .. s:tmux_reset_seq .. shellescape(a:text))
endfunction
