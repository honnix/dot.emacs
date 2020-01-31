(use-package sh-script
  :ensure t
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zshrc\\'" . sh-mode)
         ("\\.zshenv\\'" . sh-mode))
  :bind ([remap sh-case] . comment-dwim))
