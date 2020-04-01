;;; 07java.el --- java

;;; Commentary:

;;; Code:

(require 'cc-mode)

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp-deferred))

;;; 07java.el ends here
