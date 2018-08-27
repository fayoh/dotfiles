# Antigen
source ~/bin/antigen.zsh
antigen init ~/.antigenrc

setopt histignorealldups
setopt share_history
setopt longlistjobs
setopt nobeep

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

#automatically remove duplicates from these arrays
typeset -U path

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
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

#Pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Enable local override
if [ -f ~/.zshrc_local ]; then
    source ~/.zshrc_local
fi
