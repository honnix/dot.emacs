;;; 27go.el --- go

;;; Commentary:

;;; Code:

(use-package go-guru
  :ensure t
  :init
  (setq go-guru-scope "."))

(use-package go-mode
  :ensure t
  :requires go-guru
  :hook (go-mode . my-go-mode-hook)
  :init
  (setq indent-tabs-mode 1
        tab-width 4)
  :bind (("C-c C-f c" . go-remove-unused-imports))
  :config
  (defun my-go-mode-hook ()
	(add-to-list 'company-backends 'company-go)
	(if (not (string-match "go" compile-command))
		(set (make-local-variable 'compile-command)
			 "go build -v && go test -v && go vet"))
	(go-guru-hl-identifier-mode)))

;;; 27go.el ends here
