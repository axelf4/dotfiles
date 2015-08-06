setopt AUTO_CD # Just type 'dir' instead of 'cd dir'
export EDITOR="vim" # Use vim as default editor

autoload -U colors && colors

PROMPT="%{%K{blue}%}%{%0F%}%~%{%4F%k%}%{%f%} "
# RPROMPT="%{%3F%}%{%f%0F%K{yellow}%}git%{$reset_color%}"
