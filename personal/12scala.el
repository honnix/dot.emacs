(use-package scala-mode
  :ensure t
  :mode ("\\.scala\\'" "\\.sbt\\'")
  :bind ("C-c C-c" . comment-dwim)
  :config
  (unbind-key "C-c C-c" scala-mode-map))
