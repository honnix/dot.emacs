;; Use '%' to match parentheses
(global-set-key (kbd "C-%") 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise (not now) insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;	(t (self-insert-command (or arg 1)))))
    ))

;; Always use "C-c C-c" to make region (un)comment
(global-set-key (kbd "C-c C-c") 'comment-dwim)

;; Auto insert
(require 'autoinsert)
(auto-insert-mode 1)
(setq auto-insert t)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (expand-file-name "~/.emacs.d/personal/templates/"))
(define-auto-insert "\\.hh\\'" "template.hh")
(define-auto-insert "\\.cc\\'" "template.cc")

;; Close compilation window if sucessing
(setq compilation-finish-functions
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "COMPILATION ERROR!, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (if (string-match "*compilation*" buf)
              (run-at-time 0.5 nil 'delete-windows-on buf))
          (message "no compilation error."))))
