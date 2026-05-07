;;; 09rust.el --- rust -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t
  ;; :ensure-system-package rls  ; install rls (or rust-analyzer) and re-enable
  :hook ((rust-mode . lsp-deferred)
         (before-save . my-rust-before-save-hook))
  :config
  (defun my-rust-before-save-hook ()
    (when (eq major-mode 'rust-mode)
      (lsp-format-buffer))))


;;; 09rust.el ends here
