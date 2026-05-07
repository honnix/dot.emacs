;;; 05protobuf.el --- protobuf -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package protobuf-mode
  :ensure t
  :preface
  (defun my-protobuf-hook ()
    (setq c-basic-offset 2))
  :hook (protobuf-mode . my-protobuf-hook))

;;; 05protobuf.el ends here
