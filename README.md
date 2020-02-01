# My Emacs Configurations

Everything included in this repository has been verified to work with
[Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus) latest version.

## Structure

* `3rd` contains packages from 3rd parties', and these packages can not be found in
  emacs package repositories or the versions there are too old

* `elpa` contains packages from emacs package repositories

* `personal` contains all my own configurations, they are ordered and invoked by dot.emacs

* `dot.emacs` is actually the `.emacs`

## How to

1. `cd $HOME`

2. `git clone git@github.com:honnix/dot.emacs.git .emacs.d`

3. `ln -s .emacs.d/dot.emacs .emacs`
