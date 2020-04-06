;;; 06python.el --- python

;;; Commentary:

;;; Code:

(use-package elpy
  :ensure t
  ;; :requires bind-key
  :hook ((python-mode . lsp-deferred)
         (before-save . my-py-before-save-hook))
  :init
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-offset 4)
  :config
  (defun my-py-before-save-hook ()
    (when (eq major-mode 'python-mode)
      (lsp-format-buffer)
      (lsp-organize-imports)))

  (elpy-enable))
  ;; (unbind-key "C-c C-c" python-mode-map)
  ;; (unbind-key "C-c C-c" elpy-mode-map))

(use-package python-pytest
  :ensure t)

;;; 06python.el ends here
