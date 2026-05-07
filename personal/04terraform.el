;;; 04terraform.el --- terraform -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

;;; 04terraform.el ends here
