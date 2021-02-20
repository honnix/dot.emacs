;;; 08ruby.el --- ruby

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :ensure nil
  :hook ((ruby-mode . lsp-deferred)))

(use-package lsp-solargraph
  :ensure nil)

;;; 08ruby.el ends here
