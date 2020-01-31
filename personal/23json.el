(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :init
  (defun reset-indent-level ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))
  :hook (json-mode . reset-indent-level))
