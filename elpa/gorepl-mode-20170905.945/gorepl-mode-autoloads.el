;;; gorepl-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gorepl-mode" "gorepl-mode.el" (0 0 0 0))
;;; Generated autoloads from gorepl-mode.el

(autoload 'gorepl-mode "gorepl-mode" "\
A minor mode for run a go repl on top of gore

If called interactively, enable Gorepl mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gorepl-mode" '("gorepl-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gorepl-mode-autoloads.el ends here
