(autoload 'js2-mode "js2-mode" nil t)
(autoload 'json-mode "json-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
