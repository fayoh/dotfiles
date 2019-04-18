#!/bin/bash
# Really dumb script (as in no checks or anything) to download
# a file if it does not already exist
#
# If a third parameter is passed (with any value) the downloaded file
# gets its executable bit set.

# Usage ./clone.sh [-x] url destination

function HELP {
    echo "Usage: clone.sh [-x] repo-url destination"
}

while getopts hx FLAG; do
    case $FLAG in
	x)
	    SETEXEC=1
	    ;;
	h)
	    HELP
	    exit
	    ;;
	\?)
	    echo -e \\n"Option -${BOLD}${OPTARG} not allowed."
	    HELP
	    exit
	    ;;
    esac
done
shift $((OPTIND -1))

if [ ! "$2" ]; then
    HELP
    exit
fi


if [  -f "$2" ]; then
    echo "Target exists, skipping download"
else
    curl --compress --create-dirs -L "$1" -o "$2"
    if [ "$SETEXEC" ]; then
	chmod +x "$2"
    fi
fi
