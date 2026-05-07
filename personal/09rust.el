;;; 09rust.el --- rust -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t
  ;; :ensure-system-package rls  ; install rust-analyzer and re-enable
  :preface
  (defun my-rust-before-save-hook ()
    (when (eq major-mode 'rust-mode)
      (lsp-format-buffer)))
  :hook ((rust-mode . lsp-deferred)
         (before-save . my-rust-before-save-hook)))


;;; 09rust.el ends here
