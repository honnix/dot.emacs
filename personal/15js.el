(use-package js2-mode
  :ensure t
  :after json-mode
  :mode "\\.js\\'"
  :init
  (setq-default js2-basic-offset 2
                js-indent-level 2))
