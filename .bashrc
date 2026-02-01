[[ $- = *i* ]] || return # Skip if noninteractive

HISTIGNORE='&:??: *:pwd:clear:exit'
PROMPT_DIRTRIM=2 # Trim long paths in the prompt
export VISUAL='emacsclient --alternate-editor= --tty --' # Set Emacs as editor
export ERL_AFLAGS='+pc unicode -kernel shell_history enabled'

_prompt_git() {
	# If inside a Git repository, print symbolic ref; remote tracking
	# branch or tag; or short SHA-1 of the last commit.
	git rev-parse --is-inside-work-tree >&2 \
		&& { printf \ ; git symbolic-ref --quiet --short HEAD \
				 || git describe --all --exact-match \
				 || git rev-parse --short HEAD \
				 || echo unknown; }
} 2>/dev/null
PS1='$(e=$?; [[ $e -ne 0 ]] && echo -n "\[\e[31m\]$e ")\[\e[36m\]\w\[\e[1;33m\]$(_prompt_git)\[\e[0m\] ❯ '

mkcd() { mkdir -p "$@" && cd -- "${@: -1}"; }

# Prints an overview of the current directory
status() {
	ls
	2>/dev/null git status --short --branch || true
}

stty -ixon susp undef # Disable XON/XOFF flow control
bind -x '"\C-z": fg' # Make Control-z toggle fore-/background
bind '"\C-n": dabbrev-expand'

bind 'Return: "\xff0\n"'
bind -x '"\xff0": [[ $READLINE_LINE ]] 2>/dev/null || READLINE_LINE=\ status'

alias e=\$VISUAL
alias o=xdg-open
alias ..='cd ..'
alias ...='cd ../..'
alias cfg='git --git-dir="$HOME"/.dotfiles --work-tree="$HOME"'
alias less='less --RAW-CONTROL-CHARS --ignore-case'
alias ip='ip -color=auto' # Enable ip(8) color output
alias objdump='objdump --disassembler-options=intel --disassembler-color=terminal'
