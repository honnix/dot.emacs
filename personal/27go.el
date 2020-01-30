(use-package go-mode
  :ensure t
  :defer t
  :init
  (autoload 'go-mode "go-mode" nil t)
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-to-list 'company-backends 'company-go)
    (local-set-key (kbd "C-c C-f c") 'go-remove-unused-imports)
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-*") 'pop-tag-mark)
    (setq go-guru-scope ".")
    (go-guru-hl-identifier-mode)
    )
  (add-hook 'go-mode-hook #'my-go-mode-hook))
