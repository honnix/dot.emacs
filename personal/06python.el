(elpy-enable)

(eval-after-load "elpy"
  (lambda ()
     (define-key elpy-mode-map (kbd "C-c C-c") nil)))

(add-hook 'python-mode-hook
          '(lambda ()
             (my-python-mode-key-bind)))

(defun my-python-mode-key-bind ()
  (interactive)
  (local-unset-key (kbd "C-c C-c")))
