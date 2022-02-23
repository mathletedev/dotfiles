set PATH ~/bin/ $PATH
set -gx EDITOR nvim
set -g theme_color_scheme nord

alias ls="exa -a --long --git"
alias dfs="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias clean="clear; neofetch"
alias cmatrix="cmatrix -C blue"
alias clock="tty-clock -c -C 1"
alias thought="fortune | pokemonthink"
alias strat="aow | pokemonsay"

starship init fish | source
