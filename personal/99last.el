;;; 99last.el --- the last things should happen -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; =============================================================================
;; ======================== the last things should happen ======================
;; =============================================================================
;; disable things for speed

(defun nfs ()
  "Disable stuff for speed."
  (interactive)
  (global-hl-line-mode -1)
  (display-line-numbers-mode -1))

;; doom-one overrides 36 gnus-* faces with :inherit chains that create
;; cycles against the default defface specs on Emacs 31, breaking
;; make-frame in daemon mode. Strip doom-one's gnus overrides after
;; all init is done so nothing re-applies them.
(mapatoms
 (lambda (sym)
   (when (and (facep sym) (string-prefix-p "gnus-" (symbol-name sym)))
     (put sym 'theme-face
          (assq-delete-all 'doom-one (get sym 'theme-face))))))

;;; 99last.el ends here
