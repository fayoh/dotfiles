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

