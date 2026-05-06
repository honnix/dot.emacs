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
;; don't fall back to user-init-file. The actual load happens later in
;; personal/01basic.el, after package.el has initialized — custom.el
;; references packages (e.g. session) that need to be on the load path.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Frame geometry must be set before the initial frame is created;
;; defaults from after-init-hook would only affect subsequent frames.
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist
             (cons 'font (if (eq system-type 'darwin)
                             "Fira Code-12.5"
                           "Fira Code-9.5")))

(provide 'early-init)
;;; early-init.el ends here
