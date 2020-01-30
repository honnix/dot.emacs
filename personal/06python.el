(use-package elpy
  :ensure t
  :init
  :config
  (defun my-python-mode-key-bind ()
    (interactive)
    (local-unset-key (kbd "C-c C-c")))
  (define-key elpy-mode-map (kbd "C-c C-c") nil)
  (add-hook 'python-mode-hook
            '(lambda ()
               (my-python-mode-key-bind))
  (elpy-enable)))
