;;; 07java.el --- java -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cc-mode)

(use-package lsp-java
  :ensure t
  :preface
  (defun my-java-mode-hook ()
    (setq c-basic-offset 2))
  :hook ((java-mode . lsp-deferred)
         (java-mode . my-java-mode-hook)))

;;; 07java.el ends here
