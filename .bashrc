export VISUAL='emacsclient --alternate-editor= --tty --' # Set Emacs as editor
HISTIGNORE='&:??: *:pwd:clear:exit'
PROMPT_DIRTRIM=2 # Trim long paths in the prompt (requires Bash 4.x)
export ERL_AFLAGS='+pc unicode -kernel shell_history enabled'

prompt_git() {
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
PS1='$(e=$?; [[ $e -ne 0 ]] && echo -n "\[\e[31m\]$e ")\[\e[36m\]\w\[\e[1;33m\]$(prompt_git)\[\e[0m\] ❯ '

if [[ $- == *i* ]]; then
	stty -ixon susp undef # Disable XON/XOFF flow control
	bind -x '"\C-z": fg' # Make ^Z toggle between fore-/background
fi

alias e='$VISUAL'
alias ..='cd ..'
alias ...='cd ../..'
alias cfg='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
alias less='less --RAW-CONTROL-CHARS'
alias ip='ip -color=auto' # Enable ip(8) color output

# Effectively define an alias of length zero:
# Wrap in shell function since execution will not inherit DEBUG trap
__prompt_command() {
	# Check if no other command has been executed since last prompt
	[[ "$__prev_cmd" = ${FUNCNAME[0]} ]] || return 1
	ls
	git status --short --branch 2>/dev/null
}
PROMPT_COMMAND=__prompt_command
trap '__prev_cmd="$__this_cmd"; __this_cmd="$BASH_COMMAND"' DEBUG
