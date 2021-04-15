;;; 05protobuf.el --- protobuf

;;; Commentary:

;;; Code:

(use-package protobuf-mode
  :ensure t
  :hook (protobuf-mode . my-protobuf-hook)
  :config
  (defun my-protobuf-hook ()
    (setq c-basic-offset 2)))

;;; 05protobuf.el ends here
