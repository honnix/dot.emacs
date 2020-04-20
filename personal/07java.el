;;; 07java.el --- java

;;; Commentary:

;;; Code:

(require 'cc-mode)

(use-package lsp-java
  :ensure t
  :hook ((java-mode . lsp-deferred)
         (java-mode . my-java-mode-hook))
  :config
  (defun my-java-mode-hook ()
    (setq c-basic-offset 2)))

;;; 07java.el ends here
