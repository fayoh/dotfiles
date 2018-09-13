#### exa
# Exa is a "new ls" without assumptions of teletype like performace

if (( $+commands[exa] )); then
    alias l='exa --all --group-directories-first --header --long --color-scale'
    alias lg='exa --all --group-directories-first --grid --header --long --color-scale'
    alias lt='exa --all --group-directories-first --header --long --tree --color-scale --ignore-glob .git'
    function lti {
        emulate -LR zsh
        exa --all --header --long --tree --color-scale --ignore-glob ".git|$1" ${@:2}
    }
    alias ltl='exa --all --header --long --tree --color-scale --ignore-glob .git --level'
    function ltli {
        emulate -LR zsh
        exa --all --header --long --tree --color-scale --level $1 --ignore-glob ".git|$2" ${@:3}
    }
else
    # We alias gls to a git command elsewhere, so we use "command"
    # here to prevent it from being interpreted as said git command.
    if (( $+commands[gls] )); then
        alias l='command gls -AlhF --color=auto'
    else
        alias l='ls -AlhF'
    fi
    if (( $+commands[tree] )); then
        alias lt=tree
        alias ltl='tree -L'
    fi
fi
