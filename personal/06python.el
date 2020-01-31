(use-package elpy
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :bind ([remap python-shell-send-buffer] . comment-dwim)
  :config
  (elpy-enable))
