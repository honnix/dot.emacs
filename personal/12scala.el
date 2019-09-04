(add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))

(add-hook 'scala-mode-hook
          '(lambda ()
             (my-scala-mode-key-bind)))

(defun my-scala-mode-key-bind ()
  (interactive)
  (local-unset-key (kbd "C-c C-c")))
