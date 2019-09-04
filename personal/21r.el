;; ESS mode configuration (only if ess is in a nonstandard place)
(add-to-list 'load-path "~/.emacs.d/3rd/ess-12.09-2/lisp");;<<CHANGE
(autoload 'R-mode "ess-site.el" "ESS" t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
