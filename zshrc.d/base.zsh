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

#### mkdir

alias md='mkdir -p'

function mcd {
    emulate -LR zsh
    mkdir -p $@
    cd ${@[$#]}
}

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
