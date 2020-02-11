(use-package elpy
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :bind ([remap python-shell-send-buffer] . comment-dwim)
  :init
  (setq python-indent-guess-indent-offset-verbose nil
		python-indent-offset 4)
  :config
  (elpy-enable))
