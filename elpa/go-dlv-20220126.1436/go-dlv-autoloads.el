;;; go-dlv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-dlv" "go-dlv.el" (0 0 0 0))
;;; Generated autoloads from go-dlv.el

(autoload 'dlv "go-dlv" "\
Run dlv on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

(autoload 'dlv-current-func "go-dlv" "\
Debug the current program or test stopping at the beginning of the current function." t nil)

(register-definition-prefixes "go-dlv" '("go-dlv-marker-" "gud-dlv-command-name"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; go-dlv-autoloads.el ends here
