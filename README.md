# Dotfiles
My dotfiles ...

TDB

## Installation
This dotfiles repo is managed by [Dotbot](https://github.com/anishathalye/dotbot).

The supplied install script will first sync it's submodules containing
dotbot and some software that does not have official
packages. Thereafter, it will use dotbot to parse first
`default.conf.yaml` and then any supplied arguments in order appending
`.conf.yaml`.

For example, `./install home` would first run the settings in
`default.conf.yaml` and then `home.conf.yaml`

## Organisation
The base directory contains the basics, stuff I want no matter what
environment I am in.

Under site-specific are settings and sw that are only useful in a
specific setting. Think work/home/laptop.

### zshrc
To improve readability of my zshrc and simplify modifications per
setting I have created a `conf.d` like directory (`zshrc.d`) and all the
regular `.zshrc` file does is to load any zsh file it finds in that
directory.

### emacs
The `emacs.d` directory is linked in from the default configuration
file. Customisation from the site-specific directories can be done two
ways.

* For settings needed early like setting up proxies for package
installation supply a file named `early-init.el` on the load-path.
* For other configurations, put elisp files in the `conf.d` directory and they will be loaded at the end of `init.el`

## Other packages

The configuration files in this repository requires some software
packages to be installed to function correctly. Exactly what this is I leave as an exercise for the reader :)

To install all debian packages I usually want see the packages.debian file.

(Only tested using Debian 9 and Ubuntu 18.04.1 LTS)

``` shell
sudo apt-get install $(cat packages.debian|grep -v '^#')
```


To install all python modules via pip, see the packages.python file.

``` shell
pip install -r packages.python
```


### Workarounds
Qtodotxt2 does not work out of the box for me, OpenGL is not imported as it should. Add a line to `app.py`

``` python
from OpenGl import GL
```

## Manual installs
Software I like to have but is not available in the package managers I
usually use.

* lazygit https://github.com/jesseduffield/lazygit
