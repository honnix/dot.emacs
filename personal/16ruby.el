;; (require 'ruby-mode)
;; (require 'ruby-end)

;; (add-to-list 'auto-mode-alist '("\\(Rakefile\\|Gemfile\\|.gemspec$\\|\\.ru$\\)" . ruby-mode))

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (my-ruby-mode-key-bind)))

;; (defun ruby-send-buffer ()
;;   (interactive)
;;   (ruby-send-region 1 (buffer-size)))

;; (defun my-ruby-mode-key-bind ()
;;   (interactive)
;;   (local-unset-key (kbd "C-c C-c"))
;;   (local-set-key (kbd "C-c C-w") 'ruby-send-buffer))

;; (add-to-list 'load-path "~/.emacs.d/3rd/ruby-debug-extra")
;; (setq rdebug-populate-common-keys-function 'rdebug-populate-common-keys-eclipse)
;; (require 'rdebug)
