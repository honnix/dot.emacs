A powerful virtualenv tool for Emacs.  See documentation at
https://github.com/porterjamesj/virtualenvwrapper.el

POTENTIAL TODOS:
- Figure out a better way to make M-x shell work than
  advising it.  This could be done if Emacs had pre-
  and post- shell activation hooks.
- Implement the option to have eshell work in a separate
  namespace.  This would be a substantial refactor.
- Add an option for `venv-location' to be an alist.
- Propertize the venv names in the output of `venv-lsvirtualenv'
  so that clicking or pressing RET on one will switch to it.

VERSION HISTORY
20130921
- Fix a bug that caused an error if exec-path was nil (Thanks Steven Huwig).
- Fix a bug that prevented cpvirtualenv from working in eshell.
- Fix a bug in which deleted virtualenvs sometimes still showed up
  in completions.
- Eshell commands now tab complete virtualenv names where appropriate.
- mkvirtualenv and rmvirtualenv can now accept multiple names.
