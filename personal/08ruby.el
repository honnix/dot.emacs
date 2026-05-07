;;; 08ruby.el --- ruby -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :ensure nil
  :preface
  (defun my-rb-before-save-hook ()
    (when (eq major-mode 'ruby-mode)
      (lsp-format-buffer)
      (lsp-organize-imports)))
  :hook ((ruby-mode . lsp-deferred)
         (before-save . my-rb-before-save-hook)))

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode))

;;; 08ruby.el ends here
