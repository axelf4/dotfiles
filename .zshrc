setopt AUTO_CD # Just type 'dir' instead of 'cd dir'
export EDITOR="vim" # Use vim as default editor

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # Case-insensitive tab completion

autoload -U compinit && compinit
autoload -U colors && colors

# PROMPT="%{%K{blue}%}%{%0F%}%~%{%4F%k%}%{%f%} "
PROMPT="%{%5F%}%~ %{%2F%}» %{$reset_color%}"
