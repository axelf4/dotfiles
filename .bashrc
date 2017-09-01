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
	branchName="$(git symbolic-ref --quiet --short HEAD 2> /dev/null || \
		git describe --all --exact-match HEAD 2> /dev/null || \
		git rev-parse --short HEAD 2> /dev/null || \
		echo '(unknown)')";
	# Check if it's dirty (slow) via github.com/git/git/blob/355d4e173/contrib/completion/git-prompt.sh#L472-L475
	dirty=$(git diff --no-ext-diff --quiet --ignore-submodules --exit-code || echo -e "*")
	echo -e "${branchName}$dirty";
	return
}

rightprompt() {
	printf "%*s\r" $COLUMNS $(prompt_git)
}

PS1='\[\033[1;31m$(rightprompt)\]\033[36m\w\033[0m ❯ '

alias ..="cd .."
alias ...="cd ../.."
