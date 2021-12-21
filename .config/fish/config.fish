set -gx EDITOR nvim
set -g theme_color_scheme nord

alias la="ls -la"
alias dfs="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias clean="clear; neofetch"

starship init fish | source
