;;; 01basic.el --- basic config

;;; Commentary:

;;; Code:

;; =============================================================================
;; ============================ load use-package ===============================
;; =============================================================================
(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil
                exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH"))
        ;; exec-path-from-shell-arguments (list "-l"))
  :config
  (exec-path-from-shell-initialize))
;; =============================================================================
;; =============================================================================

;; =============================== customize ===================================
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (use-package cus-edit
    :preface (provide 'cus-edit)
    :config
    (load custom-file)))
;;==============================================================================
;;==============================================================================

;; =============================================================================
;; ============================ custom functions ===============================
;; =============================================================================
;; evaluate and replace the text
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "invalid expression")
           (insert (current-kill 0)))))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "refreshed open files"))

(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (point-to-register 8))
(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(defun my-newline ()
  "New line after current line."
  (interactive)
  (move-end-of-line 1)
  (newline))
(defun my-newline-pre ()
  "New line before current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1))

(defun my-select-current-line ()
  "Select current line."
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise (not now) insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;       (t (self-insert-command (or arg 1)))))
    ))

(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

;; =============================================================================
;; =============================================================================

;; =============================================================================
;; ============================== key bindings =================================
;; =============================================================================
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-.") 'ska-point-to-register)
(global-set-key (kbd "C-,") 'ska-jump-to-register)
(global-set-key (kbd "C-o") 'my-newline)
(global-set-key (kbd "C-S-o") 'my-newline-pre)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key (kbd "C-c h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c n") 'highlight-symbol-next)
(global-set-key (kbd "C-c p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c l") 'my-select-current-line)
;; (global-set-key (kbd "C-c C-c") 'comment-dwim)
(global-set-key (kbd "C-%") 'match-paren)
(global-set-key (kbd "<s-left>") 'backward-global-mark)
(global-set-key (kbd "<s-right>") 'forward-global-mark)

;; (global-set-key (kbd "C-a") 'beginning-of-line)
;; (global-set-key (kbd "C-e") 'end-of-line)
;; =============================================================================
;; =============================================================================

;; =============================================================================
;; ============================== set variables ================================
;; =============================================================================
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)

(setq locale-coding-system 'utf-8)
(setq shell-file-name "/usr/local/bin/zsh")

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(setq user-full-name "Hongxin Liang"
      user-login-name "honnix"
      user-mail-address "hxliang1982@gmail.com")

;; scroll before we reach the end of the screen
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

;; https://github.com/emacs-lsp/lsp-mode#performance
(setq gc-cons-threshold 200000000
      read-process-output-max (* 2048 1024))

;; y/n for short
(fset 'yes-or-no-p 'y-or-n-p)

;;; 01basic.el ends here
