" Quickfix window is global therefore we also global
"
" TODO:
" * Add support for other TMUX stuff
" * Add support for Vim8 :terminal
" * Add fallback to synchronous :make, :!, ...
" * Add a consistent interface for all backends
" * Expand command
"
" Provide SendKeys and RunCommand which can optionally put in quickfix window
"
" Add completion for compiler command, tmux pane name, ...
"
" Have a pool of runners ready and only create a new one if noone are
" available
"
" Automatically find matching compiler and provide fallback generic
" errorformat
"
" TODO Come up with name, Thinking maybe Aron (like pokemon) for Async Run
"
" TODO Create function for aborting a job

let s:pending = {} " Dict of pane_id to requests which have yet to finish

function! s:Shellpipe(file) abort
	" Any "%s" are interpolated with the name of the temporary file,
	" which is appended automatically if no %s appear
	return empty(&shellpipe) ? '' : &shellpipe =~# '%s' ? printf(&shellpipe, a:file) : &shellpipe . ' ' . a:file
endfunction

" The exit status variable for the shell in use
let s:exitstatus = &shellxquote ==# '"' ? '%ERRORLEVEL%' : &shell =~# 'csh\|fish' ? '$status' : '$?'

function! s:Subshell(c) abort
	return &shell =~# 'fish' ? 'begin; ' . a:c . '; end' : '(' . a:c . ')'
endfunction

" Have one function that creates a runner with an optional command
" Takes option if want to grab focus
" and optional command
"
" And one function that uses some combination of the above two to do the whole
" Quickfix thingy-majiggy
"
" Either provide a target or it will create a short-lived one

" TODO Maybe rename to targets
" let g:asyncrun#runners = []

function! asyncrun#NewRunner(opts) abort
	let pane = system('tmux ' . (get(a:opts, 'background', 0) ? 'new-window' : 'split-window')
				\ . ' ' . (get(a:opts, 'focus', 0) ? '' : '-d')
				\ . ' ' . (get(a:opts, 'vertical', 0) ? '-v' : '-h')
				\ . ' -P -F "#{pane_id}" ' . get(a:opts, 'cmd', ''))[:-2] " Remove NL byte

	return { 'pane': pane, }
endfunction

function! asyncrun#Run() abort
	" Write all if requested by user
	if &autowrite || &autowriteall | silent! wall | endif

	let outfile = tempname()
	echom 'Outfile: ' . outfile
	" let script = 'gcc main.c'
	let script = 'true'
	let request = {
				\ 'file': outfile,
				\ 'directory': getcwd(),
				\ }
	call s:SetupCallback(request)

	" let cmd = shellescape(s:Subshell(script . '; echo ' . s:exitstatus  . ' > ' . fnameescape(outfile . '.complete')) . ' ' . s:Shellpipe(outfile))
	let cmd = shellescape(script . '; echo ' . s:exitstatus  . ' > ' . fnameescape(outfile . '.complete'))

	if empty(g:asyncrun#runners) || 1
		let runner = asyncrun#NewRunner({
					\ 'cmd': cmd,
					\ })
		let simple = 1
	else
		let runner = g:asyncrun#runners[0]
		let simple = 0
	endif
	let request.runner = runner

	let filter = 'sed'
	let uname = system('uname')[0:-2]
	if uname ==# 'Darwin'
		let filter = '/usr/bin/sed -l'
	elseif uname ==# 'Linux'
		let filter .= ' -u'
	endif
	if !simple | let filter .= " -e 1d" | endif " Remove line of command
	let filter .= " -e \"s/\r\r*$//\" -e \"s/.*\r//\""
	let filter .= " -e \"s/\e\\[K//g\" "
	let filter .= " -e \"s/.*\e\\[2K\e\\[0G//g\""
	let filter .= " -e \"s/.*\e\\[?25h\e\\[0G//g\""
	let filter .= " -e \"s/\e\\[[0-9;]*m//g\""
	let filter .= " -e \"s/\017//g\""
	let filter .= " > " . outfile . ""
	" call system('tmux pipe-pane -t ' . runner.pane . " 'cat > " . outfile . "'")
	call system('tmux pipe-pane -t ' . runner.pane . ' ' . shellescape(filter))

	" Send reset sequence followed by command and enter
	if !simple
		call asyncrun#SendKeys(runner, s:tmux_reset_seq . cmd . ' Enter')
	endif

	let s:pending[pane] = request
	call s:TmuxPoll() " Pick up on any quick commands
endfunction

" Sends the specified keys to the target
" TODO Try to have it use Vim format of specifying keys
function! asyncrun#SendKeys(runner, keys) abort
	call system('tmux send-keys -t ' . a:runner.pane . ' ' . a:keys)
endfunction

function! s:Complete(request, status) abort
	let runner = a:request.runner
	call system('tmux pipe-pane -t ' . runner.pane) " Close the pipe

	" Populate quickfix window
	" FIXME Finalize!
	" call setqflist([], 'r') " Clear quickfix list ONLY neccessary
	execute 'cgetfile ' a:request.file

	botright copen " Open quickfix window
	let w:quickfix_title = (a:status ? 'Failed! ' : 'OK! ') . a:request.file " TODO
	" wincmd p " Go to previous window
	checktime " Poll buffers changed outside of Vim
	redraw!
	silent doautocmd ShellCmdPost
endfunction

" Prompts the user for a target window, referring to Vim's usage of the word
"
" After this function returns and in the absence of errors, the current
" buffer/window is guaranteed to belong to the prompt.
function! asyncrun#PromptWindow(Cb) abort
	let [head, windowLabels] = s:TmuxWinlayout()

	" TODO Look into using syntax region for cell lookup based on cursor pos
	" Recursive procedure for drawing cells
	function! DrawCell(node, x, y, w, h, windowLabels) abort
		let type = a:node[0]
		if type ==# 'leaf'
			let text = a:windowLabels[a:node[1]]
			execute 'normal! ' . (a:y + a:h / 2) . 'G' . (a:x + (a:w - strdisplaywidth(text)) / 2) . '|R' . text . "\<Esc>"
		else
			let nchildren = len(a:node[1]) | let l = type ==# 'row' ? a:w : a:h
			let percell = (l - (nchildren - 1)) / nchildren | let rem = (l - (nchildren - 1)) % nchildren
			let [cx, cy] = [a:x, a:y]
			for i in range(nchildren)
				let [cw, ch] = type ==# 'row' ? [percell + (i < rem), a:h] : [a:w, percell + (i < rem)]
				call DrawCell(a:node[1][i], cx, cy, cw, ch, a:windowLabels)
				let [cx, cy] += type ==# 'row' ? [cw, 0] : [0, ch]
				if i != nchildren - 1 | execute 'normal! ' . cy . 'G' . cx . "|\<C-V>" . (type ==# 'row' ? (ch - 1) . 'jr║' : (cw - 1) . 'lr═') | endif
				let [cx, cy] += type ==# 'row' ? [1, 0] : [0, 1]
			endfor
		endif
	endfunction
	silent keepalt new __Window_Select__ " Create new window
	setlocal buftype=nofile bufhidden=hide noswapfile nobuflisted nonumber nolist modifiable
	let b:Cb = a:Cb " Store callback in buffer
	let virtualedit = &virtualedit | set virtualedit=all " Enable 'virtualedit'
	silent normal! ggdGg$
	let [w, h] = [virtcol('.'), winheight(0)]
	for i in range(h - 1) | call append(0, '') | endfor " Fill buffer with lines
	silent call DrawCell(head, 1, 1, w, h, windowLabels)
	let &virtualedit = virtualedit " Restore 'virtualedit'
	setlocal nomodifiable
	silent execute "normal! gg0/\\d\\+\<CR>"

	function! s:WindowSelectEnter() abort
		let word = expand('<cword>') " Get word under cursor
		if word =~# '\D' | return | endif
		try | call b:Cb('%' . word) | finally | quit | endtry
	endfunction
	nnoremap <script> <buffer> <nowait> <silent> <CR> :call <SID>WindowSelectEnter()<CR>
	nnoremap <script> <buffer> <nowait> <silent> <C-n> /\d\+<CR>
	nnoremap <script> <buffer> <nowait> <silent> <C-p> ?\d\+<CR>

	augroup asyncrunprompt
		autocmd! * <buffer>
		autocmd WinLeave <buffer> quit
	augroup END
endfunction

" Tmux {{{
let s:tmux_reset_seq = 'q C-u ' " Key sequence for leaving copy-mode and clearing prompt

function! s:TmuxPoll() abort
	if s:HasCallback() | return | endif

	let panes = split(system('tmux list-panes -a -F "#{pane_id}"'), '\n')
	for [pane, request] in items(s:pending)
		if index(panes, pane) == -1
			call remove(s:pending, pane)
			let status = readfile(request.file . '.complete', '', 1)[0]
			call s:Complete(request, status)
		endif
	endfor
endfunction

augroup asyncrun
	autocmd!
	autocmd VimResized * call s:TmuxPoll()
augroup END

" Parse tmux window layout into format compatible with winlayout()
function! s:TmuxWinlayout() abort
	let layout = system('tmux display-message -p "#{window_layout}"')[5:-2] " Trim checksum & NL
	let stack = []
	while layout !~# '^\s*$' " While layout desc is not empty
		" Parse layout cell: window ptr and split type (see layout-custom.c)
		let [str, wp, sep, layout; rest] = matchlist(layout, '^\d\+x\d\+,\d\+,\d\+\%(,\(\d*\)x\@!\)\?\([{[\_$]\|[,}\]]\+\)\(.*\)$')
		let type = sep ==# '[' ? 'col' : sep ==# '{' ? 'row' : 'leaf'
		let node = [type, type ==# 'leaf' ? wp : []]
		if empty(stack) | let head = node | else | call add(stack[-1][1], node) | endif " Add to parent
		if type !=# 'leaf' | call add(stack, node) | endif " Push to parent stack
		for sign in split(sep, '\zs')
			if sign ==# '}' || sign ==# ']' | unlet stack[-1] | endif
		endfor
	endwhile

	" Get names of commands running in panes
	let panes = split(system('tmux list-panes -F "#{pane_id}/#{pane_current_command}"'), '\n')
	let windowLabels = {}
	for p in panes
		let [pane_id, command] = split(p, '/')
		let windowLabels[pane_id[1:]] = pane_id[1:] . ' ' . command
	endfor

	return [head, windowLabels]
endfunction
" }}}

" TODO Try to do some templating macro magic conditional compilation
function! s:HasCallback() abort
	return 1
endfunction

function! s:SetupCallback(request) abort
	call system('mkfifo ' . shellescape(a:request.file . '.complete'))
	let cmd = ['head', '-1', a:request.file . '.complete']
	let Cb = {ch, msg -> s:Complete(a:request, msg)}
	if exists('*job_start')
		call job_start(cmd, {'callback': Cb})
	else
		return ''
	endif
endfunction

" Below are the implementations of provided commands and operators
"
" XXX Consider moving any of the below items to plugin/asyncrun.vim that don't
" depend on script-local items

" Called by the command :AsyncRun.
function! asyncrun#AsyncRunCommand(args, mods) abort
	" Skip trailing comments and parse optional bar
	" TODO make this smarter, bar could be in string etc. (wild thought use
	" Vimscript syntax highlighting for this purpose?)
	let [str, cmd, after] = matchlist(a:args, '^\([^"|]*\)|\?\([^"]*\)"\?.*$')[:2]
	echom cmd
	echom after

	let vertical = a:mods =~# 'vertical'
	echom 'Vertical: ' . vertical
endfunction

function! asyncrun#SendText(pane_id, text) abort
	" echom 'pane_id: ' . a:pane_id . ' text: ' . a:text
	call asyncrun#SendKeys({'pane': a:pane_id}, s:tmux_reset_seq . shellescape(a:text))
endfunction

" TODO Try to make it into one unified function for operator in visual/normal
" and command with range
" Enters the specified text into a REPL for example
" Buffer-local variable for storing last used pane
function! asyncrun#SlimeOperator(type, ...) abort
	let sel_save = &selection | let &selection = "inclusive" | let reg_save = @@
	" If invoked from Visual mode, use gv command.
	silent execute 'normal! ' . (a:0 ? 'gv' : '`[' . (a:type ==# 'line' ? 'V': 'v') . '`]') . 'y'
	let text = @@ | let &selection = sel_save | let @@ = reg_save
	" XXX Maybe use tmux paste-buffer for large texts
	" TODO (Optionally) Strip leading whitespace and empty lines
	call asyncrun#PromptWindow({pane_id -> asyncrun#SendText(pane_id, text)})
endfunction

function! asyncrun#WindowComplete(A, L, P) abort
	" Cannot open windows with command-line window open, so do not
	if empty(getcmdwintype())
		call asyncrun#PromptWindow({->0})
		redraw!
		autocmd asyncrunprompt CmdlineLeave <buffer> quit | autocmd! asyncrunprompt
	endif
	return ''
endfunction
