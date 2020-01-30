(use-package js2-mode
  :ensure t
  :defer t
  :requires json-mode
  :init
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq-default js2-basic-offset 2
                js-indent-level 2))
