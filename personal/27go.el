(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init
  (setq go-guru-scope ".")
  (setq-default indent-tabs-mode 1)
  (setq-default tab-width 4)
  :bind (("C-c C-f c" . go-remove-unused-imports)
         ("M-." . godef-jump)
         ("M-*" . pop-tag-mark))
  :config
  (add-to-list 'company-backends 'company-go)
  (if (not (string-match "go" compile-command))
	  (set (make-local-variable 'compile-command)
		   "go build -v && go test -v && go vet"))
  (go-guru-hl-identifier-mode))
