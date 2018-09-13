#!/bin/sh
# Really dumb script (as in no checks or anything) to clone a repo if
# the directory does not already exist

# Usage ./clone.sh repo-url destination
if [ ! -d "$2" ]; then
    git clone -q "$1" "$2"
fi
