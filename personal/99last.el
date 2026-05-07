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

;;; 99last.el ends here
