# Antigen
# This is an extension loading system for zshell
source ~/.local/bin/antigen.zsh
antigen init ~/.antigenrc

# Enable parameter expansion and other substitutions in the $PROMPT.
setopt prompt_subst

# Load some associative arrays (color, fg, and bg) that give us
# convenient access to color-changing escape codes.
autoload -U colors && colors

#### History
# Only save a command once even if repeated
setopt histignorealldups

# Don't save commands to the history if they start with a leading
# space. This is useful if you have to pass a password as a parameter
# to a command.
setopt hist_ignore_space

# All zsh sessions share the same history file. Timestamps are also
# recorded for each command.
setopt share_history

# Use OS-provided locking mechanisms for the history file, if
# available. The manual says this might improve performance and
# decrease the chance of corruption.
setopt hist_fcntl_lock

# Remove superfluous whitespace when saving commands to the history.
setopt hist_reduce_blanks

# Keep history forever (within reason)
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/.zsh_history

#### Globbing

# This makes globs case-insensitive.
unsetopt case_glob

# This makes globbing regexes case-insensitive.
unsetopt case_match

# Allow globs to match dotfiles.
setopt glob_dots

# Sort numeric filenames numerically, instead of lexicographically.
setopt numeric_glob_sort

### Filesystem navigation

# This makes it so that when you cd to a new directory, the old
# directory is saved in the directory stack (view with dirs or ds).
setopt autopushd

# This makes it so that "cd -n" gives you the directory you were in n
# cd's ago, instead of the nth directory you have visited in the shell
# session. (You can use "cd +n" to recover the latter behavior.)
setopt pushdminus

# This makes it so that the working directory path is automatically
# fully resolved. This means that symlink components will be followed,
# and capitalization will be corrected if you are on a
# case-insensitive filesystem.
setopt chaselinks

# If you just type in a directory name, cd to it (unless it's also a
# valid executable name).
setopt autocd

# This alias is a convenient way to list the last few directories
# visited, with their numbers. You can then use the 'cd -n' aliases to
# jump to those directories.
alias ds='dirs -v | head -10'


#### Other
# Print job notifications in the long format by default
setopt longlistjobs

# Don't try to beep at me!
setopt nobeep

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

#automatically remove duplicates from these arrays
typeset -U path

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

#### mkdir

alias md='mkdir -p'

function mcd {
    emulate -LR zsh
    mkdir -p $@
    cd ${@[$#]}
}

#### Completion
# Use modern completion system
autoload -Uz compinit
compinit

# If a completion is performed with the cursor within a word, and a
# full completion is inserted, the cursor is moved to the end of the
# word. That is, the cursor is moved to the end of the word if either a
# single match is inserted or menu completion is performed.
setopt always_to_end

# When the current word has a glob pattern, do not insert all the
# words resulting from the expansion but generate matches as for
# completion and cycle through them like MENU_COMPLETE. The matches
# are generated as if a '*' was added to the end of the word, or
# inserted at the cursor when COMPLETE_IN_WORD is set. This actually
# uses pattern matching, not globbing, so it works not only for files
# but for any completion, such as options, user names, etc.
setopt glob_complete

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Complete hostnames in .ssh/known_hosts
zstyle -e ':completion::*:hosts' hosts 'reply=($(sed -e "/^#/d" -e "s/ .*\$//" -e "s/,/ /g" /etc/ssh_known_hosts(N) ~/.ssh/known_hosts(N) 2>/dev/null | xargs) $(grep \^Host ~/.ssh/config(N) | cut -f2 -d\  2>/dev/null | xargs))'

# Let my own binaries override system ones
export PATH="${HOME}/bin:${HOME}/.local/bin:${PATH}"

# GLOBAL
export GTAGSLABEL=pygments
export GTAGSCONF="${HOME}/.local/etc/gtags.conf"

# Editor
termedit='emacsclient -t --alternate-editor=""'
guiedit='emacsclient -c --alternate-editor=""'
alias e=${termedit}
alias ec=${guimedit}
alias suedit='sudo nano'
export EDITOR=${termedit}
export VISUAL=${termedit}

# Pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT=true # Don't replace my prompt

# Somewhere this gets set to latin1 and screws up newer formatting
unset LESSCHARSET

# Enable local override
if [ -f ~/.zshrc_local ]; then
    source ~/.zshrc_local
fi
