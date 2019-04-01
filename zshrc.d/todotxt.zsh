if type todo-txt > /dev/null
then
    alias t=todo-txt
else
    echo "Error, todo-txt not found."
    return 127
fi

export TODOTXT_CFG_FILE="$HOME/.config/todotxt/config"
export TODO_ACTIONS_DIR="$HOME/.config/todotxt/actions"
export TODOTXT_DEFAULT_ACTION="ls"

if which pygmentize > /dev/null
then
    export TODOTXT_FINAL_FILTER="pygmentize -l todotxt"
else
    echo "Warning, pygmentize not found."
fi

compdef _todo.sh todo-txt todo.sh t
