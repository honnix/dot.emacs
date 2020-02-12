(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (defun c-setup-paragraph-variables ())
  (setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-global-externs '("module" "require" "setTimeout" "clearTimeout" "setInterval"
                                     "clearInterval" "location" "__dirname" "console" "JSON" "window"
                                     "process" "fetch"))
  (setq-default js2-strict-trailing-comma-warning t
                js2-strict-inconsistent-return-warning nil
                js2-basic-offset 2
                js-indent-level 2
                indent-tabs-mode nil)
  (setq-default objc-font-lock-extra-types))

(use-package prettier-js
  :ensure t
  :ensure-system-package prettier
  :after js2-mode
  :hook (js2-mode . prettier-js-mode))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'"
  :magic ("import React" . rjsx-mode)
  :after js2-mode)

(use-package js2-refactor
  :ensure t
  :after js2-mode)

(use-package json-mode
  :ensure t
  :after js2-mode
  :init
  (setq-default js-indent-level 2))
