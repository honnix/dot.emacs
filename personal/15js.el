(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-global-externs '("module" "require" "setTimeout" "clearTimeout" "setInterval"
                                     "clearInterval" "location" "__dirname" "console" "JSON" "window"
                                     "process" "fetch"))
  (setq-default js2-strict-trailing-comma-warning t)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-basic-offset 2
                js-indent-level 2
				indent-tabs-mode nil)
  :config
  (use-package prettier-js
    :ensure t
    :ensure-system-package prettier)
  (use-package rjsx-mode
    :ensure t
    :mode "\\.jsx\\'"
    :magic ("import React" . rjsx-mode))
  (use-package js2-refactor
    :ensure t)
  (use-package json-mode
    :ensure t))
