;;; 17markdown.el --- markdown

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :ensure t
  :ensure-system-package multimarkdown
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . flyspell-mode)
  :init
  (setq markdown-command "multimarkdown"))

;;; 17markdown.el ends here
