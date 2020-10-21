;;; 16ts.el --- markdown

;;; Commentary:

;;; Code:


(use-package typescript-mode
  :ensure t
  :commands typescript-mode
  :hook ((typescript-mode . lsp-deferred))
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;;; 16ts.el ends here
