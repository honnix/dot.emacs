(use-package scala-mode
  :ensure t
  :defer t
  :init
  (autoload 'scala-mode "scala-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))
  :config
  (defun my-scala-mode-key-bind ()
    (interactive)
    (local-unset-key (kbd "C-c C-c")))
  (add-hook 'scala-mode-hook
            '(lambda ()
               (my-scala-mode-key-bind))))
