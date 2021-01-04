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

(use-package dap-python
  :ensure nil)

;;; 06python.el ends here
