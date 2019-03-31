" Quickfix window is global therefore we also global
"
" TODO:
" * Add support for Vim8 :terminal
" * Add fallback to synchronous :make, :!, ...
"
" Add completion for compiler command, tmux pane name, ...
"
" Automatically find matching compiler and provide fallback generic
" errorformat
"
" TODO Come up with name, Thinking maybe Aron (like pokemon) for Async Run
"
" TODO Create function for aborting a job
"
" TODO Add support for MATLAB code cells
" https://se.mathworks.com/help/matlab/matlab_prog/run-sections-of-programs.html

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

function! s:Shellredir(file) abort
	return &shellredir =~# '%s' ? printf(&shellredir, a:file) : &shellredir . ' ' . a:file
endfunction

function! s:Isolate(request, keep, ...) abort
	let keep = ['SHELL'] + a:keep
	let command = ['cd ' . shellescape(getcwd())]
	for line in split(system('env'), "\n")
		let var = matchstr(line, '^\w\+\ze=')
		if !empty(var) && var !~# '^\%(_\|SHLVL\|PWD\)$' && index(keep, var) < 0
			if &shell =~# 'csh'
				let command += split('setenv ' . var . ' ' . shellescape(eval('$' . var)), "\n")
			else
				let command += split('export ' . var . '=' .  shellescape(eval('$' . var)), "\n")
			endif
		endif
	endfor
	let temp = tempname()
	call writefile(command + a:000, temp)
	return 'env -i ' . join(map(copy(keep), 'v:val."=\"$". v:val ."\" "'), '') . &shell . ' ' . temp
endfunction

" Required to associate requests with Quickfix window
if !exists('s:requestsByFile') | let s:requestsByFile = {} | endif

" Either provide a target or it will create a short-lived one.
" Returns -1 in case of an error.
"
" TODO Consider respawn-window etc.
"
" TODO Add option to show pause prompt, and option to switch back to Vim when
" program exits, and fix option to focus
" and not capture output
function! asyncrun#Run(request) abort
	if &autowrite || &autowriteall | silent! wall | endif

	let target = get(a:request, 'target', '')
	let isshell = 1 " Whether target is a shell
	if !empty(target)
		" Convert target to an absolute ID and get current running command
		try
			let [a:request.target, current_cmd] = split(system('tmux display-message -p -t ' . target . ' "#{pane_id}/#{pane_current_command}"')[:-2], '/')
		catch /^Vim\%((\a\+)\)\=:E688/ | echo 'Failed to find target' | return -1 | endtry
		let target = a:request.target
		let isshell = &shell =~# current_cmd || current_cmd =~? 'sh' " Heuristic guess
	endif

	let a:request.file = tempname()
	let s:requestsByFile[a:request.file] = a:request
	let a:request.isshell = isshell
	let a:request.hadtarget = !empty(target) " TODO Rename

	if isshell
		call s:SetupCallback(a:request)
		" Add a delay to avoid race conditions
		let sleep = 'sync; ' . (executable('perl') ? 'perl -e "select(undef,undef,undef,0.1)" 2>/dev/null' : 'sleep 1') . '; '
		let pause = a:request.qf || !empty(target) ? '' : "; read -n 1 -srp '\e[1mPress ENTER or type command to continue\e[0m'; echo"
		let script = s:Isolate(a:request, ['TMUX', 'TMUX_PANE'], (empty(target) ? sleep : '') . a:request.cmd
					\ . '; echo ' . s:exitstatus  . ' > ' . fnameescape(a:request.file . '.complete') . pause)

		let a:request.lines = [script]
	else
		let lines = split(a:request.cmd, "\n")
		let lines = map(lines, {i, v -> trim(v)})
		let lines = filter(lines, {i, v -> !empty(v)}) " Skip empty lines
		let a:request.lines = lines
		let script = join(lines, "\n")
	endif

	let uname = system('uname')[:-2]
	let filter = uname ==# 'Darwin' ? '/usr/bin/sed -l' : 'sed' . (uname ==# 'Linux' ? ' -u' : '')
	let filter .= " -e \"s/\x1b\[[0-9;]*[mGKHFJ]\\|\r*$//g\"" " Remove ANSI escape sequences and newlines (from text wrap)
	if empty(target) | let filter .= " -e \"s/.*\r//\"" | endif
	let filter = shellescape(filter . ' > ' . a:request.file)

	let focus = get(a:request, 'focus', 0)
	if empty(target)
		" TODO Only not focus if we can poll effectively
		" Requires the operating system to support /dev/fd
		let a:request.target = system('tmux ' . (get(a:request, 'background', 0) ? 'new-window' : 'split-window')
					\ . ' ' . (focus ? '' : '-d')
					\ . ' ' . (get(a:request, 'vertical', 0) ? '-v' : '-h')
					\ . ' -PF "#{pane_id}" ' . shellescape('exec ' . script)
					\ . ' | { tee /dev/fd/3 | xargs -I {} tmux pipe-pane -t {} ' . filter
					\ . ' ' . s:Shellredir('/dev/null') . '; } 3>&1')[:-2]

		if !s:HasCallback() | let s:pending[pane] = request | endif
		call s:TmuxPoll() " Pick up on any quick commands
	else
		" XXX Maybe use tmux paste-buffer for large texts
		" Send dummy Enter to detect prompt
		call system('tmux send-keys -t ' . target . ' ' . s:tmux_reset_seq
					\ . '; tmux pipe-pane -t ' . target . ' ' . filter
					\ . '; tmux send-keys -t ' . target . ' Enter ' . shellescape(script . "\n")
					\ . (focus ? '; tmux select-pane -t ' . target : ''))

		if !isshell | call s:PollRepl(a:request) | endif
	endif
endfunction

" Tries to parse the prompt from window output captured from a REPL
"
" The g:asyncrun_max_prompt_lines variable specifies how many lines are
" considered.
"
" Returns a list with the lines of the prompt, or an empty list in the case of
" an error.
function! s:ParseReplPrompt(request) abort
	if has_key(a:request, 'prompt') | return a:request.prompt | endif

	let repl_lines = readfile(a:request.file, 'b', get(g:, 'asyncrun_max_prompt_lines', 2) + 1)[1:] " Skip first line (from Enter)
	" Remove characters from soft line breaks
	let repl_lines = map(repl_lines, {i, v -> substitute(v, ' \r', '', 'g')})
	" Detect everything up the first input line as the prompt
	let [str, i, start, end] = matchstrpos(repl_lines, '\V\^\.\*\ze' . escape(a:request.lines[0], '\') . '\$')
	if i == -1 | return [] | endif
	let repl_lines[i] = str " Shave off the command
	let a:request.prompt = repl_lines[:i]
	return a:request.prompt
endfunction

" Starts checking the REPL for request being finished
function! s:PollRepl(request) abort
	if !has('timers') | return -1 | endif

	function! ScheduleTimer(request) abort
		let a:request.replcheckcount = get(a:request, 'replcheckcount') + 1
		if a:request.replcheckcount >= 10
			echo 'REPL-check timed out!'
		else
			call timer_start(100 * a:request.replcheckcount, {timer_id -> s:PollRepl(a:request)})
		endif
	endfunction

	let prompt = s:ParseReplPrompt(a:request)
	if empty(prompt) | return ScheduleTimer(a:request) | endif
	" Using capture-pane instead because pipe-pane doesn't include the last line
	let lastlines = split(system('tmux capture-pane -pJt ' . a:request.target), "\n")[-len(prompt):]
	" Discern empty prompt as completion since empty lines are filtered
	if lastlines ==# prompt | call s:OnComplete(a:request, -1) | else | return ScheduleTimer(a:request) | endif
endfunction

let asyncrun_last_status = 0

" The argument status is the exit status or -1 if that doesn't apply
function! s:OnComplete(request, status) abort
	let a:request.status = a:status
	let g:asyncrun_last_status = a:status
	let target = a:request.target
	call system('tmux pipe-pane -t ' . target) " Close the pipe

	if a:request.hadtarget
		let lines = readfile(a:request.file, 'b')
		let lines = lines[1:] " Skip first line (from Enter)
		let prompt = s:ParseReplPrompt(a:request) | let prompt_len = len(prompt)
		" Remove lines starting with prompt (i.e. input commands)
		let i = 0
		while i < len(lines) - prompt_len + 1
			if lines[i:i + prompt_len - 1][1:] ==# prompt[:-2] && lines[i + prompt_len - 1] =~# '\V\^' . escape(prompt[-1], '\')
				call remove(lines, i, i + prompt_len - 1)
			else
				let i += 1
			endif
		endwhile
		let lines = map(lines, {i, v -> substitute(v, '.*\r', '', '')}) " Handle CR:s
		call writefile(lines, a:request.file, 'b')
	endif

	" Populate quickfix window
	execute 1 ? 'cfile' : 'cgetfile' a:request.file
	redraw!
	echo (a:status > 0 ? 'Failure:' :  a:status < 0 ? 'Completed:' : 'Success:') '!' . join(split(a:request.cmd, '\n'))

	" botright copen " Open quickfix window TODO: Remove
	" wincmd p " Go to previous window
	checktime " Poll buffers changed outside of Vim
	silent doautocmd ShellCmdPost

	if has_key(a:request, 'Cb') | call a:request.Cb(a:status) | endif
endfunction

" TODO Try to do some templating macro magic conditional compilation
function! s:HasCallback() abort
	return 1
endfunction

function! s:SetupCallback(request) abort
	call system('mkfifo ' . shellescape(a:request.file . '.complete'))
	let cmd = ['head', '-1', a:request.file . '.complete']
	let Cb = {ch, msg -> s:OnComplete(a:request, msg)}
	if exists('*job_start')
		call job_start(cmd, {'callback': Cb})
	else
		return ''
	endif
endfunction

" Returns the compiler plugin corresponding to the specified command
function! s:GetCompiler(cmd) abort
	let plugins = map(reverse(split(globpath(&runtimepath, 'compiler/*.vim'), "\n")),
				\ {i, file -> [fnamemodify(file, ':t:r'), readfile(file)]}) " Store filename without extension
endfunction

" Setting Quickfix window title requires it being open so do it here instead
function! s:InitQuickfix() abort
	let file = trim(matchstr(w:quickfix_title, '^:\%(cfile\|cgetfile\) \zs.*$'))
	if !empty(file)
		let request = s:requestsByFile[file]
		let w:quickfix_title = '!' . request.cmd
	endif
endfunction

augroup asyncrun
	autocmd!
	autocmd FileType qf if &buftype ==# 'quickfix' && empty(getloclist(winnr())) | call s:InitQuickfix() | endif
augroup END

" Prompts the user for a target window
"
" Here window refers to the usage of the word in Vim.
" After this function returns and in the absence of errors, the current
" buffer/window is guaranteed to belong to the prompt.
"
" TODO Allow user to specify creation of a new window instead of using
" exisiting
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
	call append(0, map(range(h - 1), '""')) " Fill buffer with lines
	silent call DrawCell(head, 1, 1, w, h, windowLabels)
	let &virtualedit = virtualedit " Restore 'virtualedit'
	setlocal nomodifiable
	silent execute "normal! gg0/\\d\\+\<CR>"

	function! s:WindowSelectEnter() abort
		let word = expand('<cword>') " Get word under cursor
		if word =~# '\D' | return | endif
		let Cb = b:Cb
		quit
		call Cb('%' . word)
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

let s:pending = {} " Dict of pane_id to requests which have yet to finish

function! s:TmuxPoll() abort
	if s:HasCallback() || empty(s:pending) | return | endif

	let panes = split(system('tmux list-panes -a -F "#{pane_id}"'), "\n")
	for [pane, request] in items(s:pending)
		if index(panes, pane) == -1
			call remove(s:pending, pane)
			let status = readfile(request.file . '.complete', '', 1)[0]
			call s:OnComplete(request, status)
		endif
	endfor
endfunction

augroup asyncruntmux
	autocmd!
	" When commands in tmux panes finish the panes close and Vim is resized
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
	let panes = split(system('tmux list-panes -F "#{pane_id}/#{pane_current_command}"'), "\n")
	let windowLabels = {}
	for p in panes
		let [pane_id, command] = split(p, '/')
		let windowLabels[pane_id[1:]] = pane_id[1:] . ' ' . command
	endfor

	return [head, windowLabels]
endfunction
" }}}

" Below are the implementations of provided commands and operators
"
" XXX Consider moving any of the below items to plugin/asyncrun.vim that don't
" depend on script-local items

" Called by the command :AsyncRun.
"
" TODO Allow specification of target in commands
" Have special syntax for specifying e.g. the pane in which matlab is running
function! asyncrun#AsyncRunCommand(args, mods) abort
	" Skip trailing comments and parse optional bar
	let [str, cmd, after] = matchlist(a:args, '^\([^"|]*\)|\?\([^"]*\)"\?.*$')[:2]

	" TODO Parse arguments
	" add -t argument for target

	let Cb = {target -> asyncrun#Run({'cmd': cmd,
				\ 'target': target,
				\ 'vertical': a:mods =~# 'vertical',
				\ 'background': a:mods =~# 'tab',
				\ 'qf': 1,
				\ 'Cb': {s -> s || execute(after)}})}

	" (Ab-)use the :browse modifier to prompt for target window
	if a:mods =~# 'browse' | call asyncrun#PromptWindow(Cb) | else | call Cb('') | endif
endfunction

function! asyncrun#AsyncRunComplete(A, L, P) abort
	if !exists('*getcompletion') | return [] | endif
	let start = join(reverse(split(a:L[:a:P], '\zs')), '')
	if a:A =~# '^-t='
		return filter(map(split(asyncrun#WindowComplete(a:A, a:L, a:P), "\n"), {i, v -> '-t=' . v}), {i, v -> v =~# '\V\^' . a:A})
	elseif a:A =~# '^-compiler='
		return map(getcompletion(a:A[10:], 'compiler'), {i, v -> '-compiler=' . v})
	elseif start =~# '|'
		return getcompletion(trim(a:L[strridx(a:L, '|') + 1:a:P]), 'command')
	else
		return getcompletion(a:A, 'shellcmd')
	endif
endfunction

function! asyncrun#WindowComplete(A, L, P) abort
	if empty(getcmdwintype()) && expand('@%') !=# '__Window_Select__' " Cannot open windows with command-line window open
		" TODO Add option for disabling this
		call asyncrun#PromptWindow({->0})
		redraw!
		autocmd asyncrunprompt CmdlineLeave <buffer> quit | autocmd! asyncrunprompt
	endif
	return system('tmux list-panes -F "#{pane_id}"')
endfunction
