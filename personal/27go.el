;;; 27go.el --- go

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :ensure-system-package gopls
  :hook ((go-mode . my-go-mode-hook)
		 (go-mode . lsp-deferred)
         (before-save . my-go-before-save-hook))
  :config
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  (defun my-go-before-save-hook ()
    (when (eq major-mode 'go-mode)
      (lsp-format-buffer)
      (lsp-organize-imports))))

(use-package gorepl-mode
  :ensure t
  :ensure-system-package gore
  :hook (go-mode . gorepl-mode))

(use-package gotest
  :ensure t)

;;; 27go.el ends here
