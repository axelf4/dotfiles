export EDITOR=vim # Set Vim as the default editor
HISTIGNORE="&:??: *:pwd:clear:exit"
# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

function prompt_git() {
	# Check if we're in a git repo. (fast)
	git rev-parse --is-inside-work-tree &>/dev/null || return
	# Check for what branch we're on. (fast)
	#   if… HEAD isn’t a symbolic ref (typical branch),
	#   then… get a tracking remote branch or tag
	#   otherwise… get the short SHA for the latest commit
	#   lastly just give up.
	echo " $(git symbolic-ref --quiet --short HEAD 2> /dev/null || \
		git describe --all --exact-match HEAD 2> /dev/null || \
		git rev-parse --short HEAD 2> /dev/null || \
		echo 'unknown')";
	return
}

PS1='\[\033[36m\]\w\[\033[1;31m\]$(prompt_git)\[\033[0m\] ❯ '

alias ..="cd .."
alias ...="cd ../.."

# Make ^Z toggle between fore-/background
stty susp undef
bind -x '"\C-z": fg'

alias cfg="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
