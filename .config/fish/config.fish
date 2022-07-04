set PATH ~/bin $PATH
set -gx EDITOR nvim
set -g theme_color_scheme nord

abbr -a -- - "cd -"
abbr -a -- n "kitty &"

alias ls="exa -a --long --git"
alias dfs="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias air="~/go/bin/air"
alias matrix="unimatrix -af -s 96"
alias clock="tty-clock -c"
alias thought="fortune | pokemonthink"
alias strat="aow | pokemonsay"

starship init fish | source
