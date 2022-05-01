set PATH ~/bin $PATH
set -gx EDITOR nvim
set -g theme_color_scheme nord

abbr -a -- - "cd -"

alias ls="exa -a --long --git"
alias dfs="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias clean="clear; neofetch"
alias air="~/go/bin/air"
alias cmatrix="cmatrix"
alias clock="tty-clock -c"
alias thought="fortune | pokemonthink"
alias strat="aow | pokemonsay"

starship init fish | source
