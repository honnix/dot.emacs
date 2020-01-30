(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (autoload 'markdown-mode "markdown-mode.el" nil t)
  (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
  :hook (pandoc-mode flyspell-mode))
