;;; 27go.el --- go

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :ensure-system-package gopls
  :hook ((go-mode . my-go-mode-hook)
		 (go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))))

;;; 27go.el ends here
