;;; 06python.el --- python

;;; Commentary:

;;; Code:

(use-package elpy
  :ensure t
  ;; :requires bind-key
  :hook ((python-mode . lsp-deferred)
         (elpy-mode . flycheck-mode)
         (before-save . my-py-before-save-hook))
  :init
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-offset 4)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)
        elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (defun my-py-before-save-hook ()
    (when (eq major-mode 'python-mode)
      (lsp-format-buffer)
      (lsp-organize-imports)))

  (elpy-enable))
  ;; (unbind-key "C-c C-c" python-mode-map)
  ;; (unbind-key "C-c C-c" elpy-mode-map))

(use-package python-pytest
  :ensure t)

(use-package lsp-pyls
  :ensure nil
  :init
  (setq lsp-pyls-configuration-sources ["flake8"]))

(use-package dap-python
  :ensure nil
  :init
  (setq dap-python-debugger 'debugpy)
  :config
  (defun dap-python--pyenv-executable-find (command)
    (executable-find command)))

(use-package pyvenv
  :ensure t)

;;; 06python.el ends here
