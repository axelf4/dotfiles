" Runs a command asynchronously.
"
" Functions like the attribute -bar was given however | runs in a synchronous
" context.
command -nargs=1 -complete=customlist,asyncrun#AsyncRunComplete AsyncRun call asyncrun#AsyncRunCommand(<q-args>, <q-mods>)

" TODO Add :Make command (asynchronous version of :make)

" TODO Try to make it into one unified function for operator in visual/normal
" and command with range
" Enters the specified text into a REPL for example
"
" TODO Buffer-local variable for storing last target
function! s:SlimeOperator(...) abort
	let this = expand('<sfile>')[9:] " The name of this function
	if !a:0 && mode() ==# 'n'
		return ':set operatorfunc=' . this . "\<CR>g@"
	elseif mode() ==# 'v'
		return ":\<C-U>call " . this . "(visualmode(), 0)\<CR>"
	endif

	let sel_save = &selection | let &selection = "inclusive" | let reg_save = @@
	" If invoked from Visual mode, use gv command
	silent execute 'normal! ' . (a:0 == 1 ? '`[' . (a:1 ==# 'line' ? 'V': 'v') . '`]' : 'gv') . 'y'
	let text = @@ | let &selection = sel_save | let @@ = reg_save

	function! SendText(target, text) abort
		let b:asyncrun_slime_target = a:target
		call asyncrun#Run({'target': a:target, 'cmd': a:text, 'qf': 0})
	endfunction
	if exists('b:asyncrun_slime_target') | call SendText(b:asyncrun_slime_target, text)
	else | call asyncrun#PromptWindow({target -> SendText(target, text)}) | endif
endfunction

" Operator that sends the text into a thingy
noremap <silent> <expr> <Plug>(AsyncSlimeSend) <SID>SlimeOperator()

nmap <Leader>s <Plug>(AsyncSlimeSend)
vmap <Leader>s <Plug>(AsyncSlimeSend)
nmap <Leader>ss <Plug>(AsyncSlimeSend)_
