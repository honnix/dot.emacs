;;; early-init.el --- early init -*- lexical-binding: t -*-

;;; Commentary:

;; Loaded before package.el and the GUI initializes.

;;; Code:

;; Emacs 30+ warns when loading .el files without a lexical-binding cookie.
;; Many ELPA packages (and a few hand-written files here) predate that
;; convention; suppress the warning class globally rather than patching each.
;;
;; `warning-suppress-log-types' (not -types) is the one that prevents the
;; *Messages*/*Warnings* spew. Load warnings.el up front so the variable
;; exists before any warning fires, and use a class custom.el isn't managing
;; so it survives the custom-set-variables reload that happens later.
(require 'warnings)
(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))

;; Point Custom at its own file before package init runs, so Custom
;; auto-saves (e.g. `package-selected-packages' written by package-install)
;; don't fall back to user-init-file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'early-init)
;;; early-init.el ends here
