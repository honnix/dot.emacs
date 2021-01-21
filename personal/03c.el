;;; 08c.el --- c

;;; Commentary:

;;; Code:

(use-package cc-mode
  :ensure nil)

(use-package ctypes
  :ensure t
  :config
  (ctypes-auto-parse-mode 1))

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

(use-package cc-styles
  :preface (provide 'cc-styles))

;;; 08c.el ends here
