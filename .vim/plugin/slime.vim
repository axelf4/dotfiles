" Vim plugin for sending text to other windows

noremap <unique> <silent> <expr> <Plug>SlimeSend slime#Operator()

if !hasmapto('<Plug>SlimeSend')
	nmap <Leader>s <Plug>SlimeSend
	xmap <Leader>s <Plug>SlimeSend
	nmap <Leader>ss <Plug>SlimeSend_
endif
