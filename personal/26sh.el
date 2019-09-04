(add-hook 'sh-mode-hook
          '(lambda ()
             (my-sh-mode-key-bind)))

(defun my-sh-mode-key-bind ()
  (interactive)
  (local-unset-key (kbd "C-c C-c")))
