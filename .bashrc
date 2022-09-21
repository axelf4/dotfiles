export VISUAL='emacsclient --alternate-editor= --tty --' # Set Emacs as editor
HISTIGNORE='&:??: *:pwd:clear:exit'
PROMPT_DIRTRIM=2 # Trim long paths in the prompt
export ERL_AFLAGS='+pc unicode -kernel shell_history enabled'

prompt_git() {
	# Check if inside a Git repository
	git rev-parse --is-inside-work-tree &>/dev/null || return
	# Check for what branch we're on.
	#   if... HEAD isn't a symbolic ref (typical branch),
	#   then... get a tracking remote branch or tag
	#   otherwise... get the short SHA for the latest commit
	#   lastly just give up.
	echo " $(git symbolic-ref --quiet --short HEAD 2>/dev/null \
		|| git describe --all --exact-match HEAD 2>/dev/null \
		|| git rev-parse --short HEAD 2>/dev/null \
		|| echo unknown)"
}
PS1='$(e=$?; [[ $e -ne 0 ]] && echo -n "\[\e[31m\]$e ")\[\e[36m\]\w\[\e[1;33m\]$(prompt_git)\[\e[0m\] ❯ '

mkcd() { mkdir -p "$@" && cd -- "${@: -1}"; }

# Prints an overview of the current directory.
status() {
	ls
	2>/dev/null git status --short --branch || true
}

if [[ $- == *i* ]]; then
	stty -ixon susp undef # Disable XON/XOFF flow control
	bind -x '"\C-z": fg' # Make ^Z toggle between fore-/background

	bind '"\C-m": "\xff1\xff0"'
	bind '"\xff0": accept-line'
	bind -x '"\xff1": [[ -z "$READLINE_LINE" ]] 2>/dev/null && READLINE_LINE=\ status'
fi

alias e=\$VISUAL
alias o=xdg-open
alias ..='cd ..'
alias ...='cd ../..'
alias cfg='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
alias less='less --RAW-CONTROL-CHARS'
alias ip='ip -color=auto' # Enable ip(8) color output
