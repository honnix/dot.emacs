;;; 03c.el --- c -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package cc-vars
  :preface (provide 'cc-vars)
  :hook (c-mode-common . my-c-hook-func)
  :init
  (setq c-basic-offset 4
        c-default-style (quote ((c-mode . "stroustrup")
                                (c++-mode . "stroustrup")
                                (other . "stroustrup"))))
  :config
  (defun my-c-hook-func ()
    (which-function-mode 1)
    (hide-ifdef-mode 1)))

;;; 03c.el ends here
