# Dotfiles
My dotfiles ...

TDB

## Installation
This dotfiles repo is managed by dotbot, get it from github or install
it from pypi whatever you prefer

The supplied install script will install using first the
default.conf.yaml and then any supplied arguments in order.

For example, "./install home" would first run the settings in
default.conf.yaml and then home.conf.yaml


## Other packages

To install all debian packages I usually want see the packages.debian file.
(Only tested using Debian 9 and Ubuntu 18.04.1 LTS)
"sudo apt-get install $(cat packages.debian)"

To install all python modules via pip, see the python.requirements.txt file.
"pip install -r packages.python"


## Manual installs
pyenv https://github.com/pyenv/pyenv
