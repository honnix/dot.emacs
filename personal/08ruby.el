;;; 08ruby.el --- ruby

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :ensure nil
  :hook ((ruby-mode . lsp-deferred)
         (before-save . my-rb-before-save-hook))
  :config
  (defun my-rb-before-save-hook ()
    (when (eq major-mode 'ruby-mode)
      (lsp-format-buffer)
      (lsp-organize-imports))))

(use-package lsp-solargraph
  :ensure nil)

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode))

;;; 08ruby.el ends here
