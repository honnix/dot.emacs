;;; 08rust.el --- rust

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t
  :ensure-system-package rls
  :hook ((rust-mode . lsp-deferred)
         (before-save . my-rust-before-save-hook))
  :config
  (defun my-rust-before-save-hook ()
    (when (eq major-mode 'rust-mode)
      (lsp-format-buffer))))


;;; 08rust.el ends here
