;;; 17markdown.el --- markdown

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :ensure t
  :ensure-system-package pandoc
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . flyspell-mode)
         (text-mode . auto-fill-mode))
  :init
  (setq markdown-command "pandoc"))

;;; 17markdown.el ends here
