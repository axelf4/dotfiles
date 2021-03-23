export VISUAL='emacsclient --alternate-editor= --tty --' # Set Emacs as editor
HISTIGNORE='&:??: *:pwd:clear:exit'
PROMPT_DIRTRIM=2 # Trim long paths in the prompt (requires Bash 4.x)

function prompt_git() {
	# Check if we're in a git repo. (fast)
	git rev-parse --is-inside-work-tree &>/dev/null || return
	# Check for what branch we're on. (fast)
	#   if… HEAD isn’t a symbolic ref (typical branch),
	#   then… get a tracking remote branch or tag
	#   otherwise… get the short SHA for the latest commit
	#   lastly just give up.
	echo " $(git symbolic-ref --quiet --short HEAD 2> /dev/null \
		|| git describe --all --exact-match HEAD 2> /dev/null \
		|| git rev-parse --short HEAD 2> /dev/null \
		|| echo 'unknown')"
}

PS1='\[\e[36m\]\w\[\e[1;33m\]$(prompt_git)\[\e[0m\] ❯ '

alias e='$VISUAL'
alias ..='cd ..'
alias ...='cd ../..'

if [[ $- == *i* ]]; then
	stty -ixon susp undef # Disable XON/XOFF flow control
	bind -x '"\C-z": fg' # Make ^Z toggle between fore-/background
fi

alias cfg='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
