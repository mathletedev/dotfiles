set PATH ~/bin/ $PATH
set -gx EDITOR nvim
set -g theme_color_scheme nord

alias ls="exa -a --long --git"
alias dfs="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias clean="clear; neofetch --ascii_colors 4 4 4 4 4 4 --colors 5 5 2 6 6 7"

starship init fish | source
