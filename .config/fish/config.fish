set PATH ~/bin/ $PATH
set -gx EDITOR nvim
set -g theme_color_scheme nord

alias ls="exa -a --long --git"
alias dfs="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias clean="clear; neofetch"

starship init fish | source
