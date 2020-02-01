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
        exec-path-from-shell-arguments (list "-l"))
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
;	(t (self-insert-command (or arg 1)))))
    ))
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
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c n") 'highlight-symbol-next)
(global-set-key (kbd "C-c p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c l") 'my-select-current-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)
(global-set-key (kbd "C-%") 'match-paren)

;; (global-set-key (kbd "C-a") 'beginning-of-line)
;; (global-set-key (kbd "C-e") 'end-of-line)
;; =============================================================================
;; =============================================================================

;; =============================================================================
;; ============================== set variables ================================
;; =============================================================================
(setq-default indent-tabs-mode nil)

(setq locale-coding-system 'utf-8)
(setq shell-file-name "/usr/local/bin/zsh")
(setq tab-width 4)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(setq user-full-name "Hongxin Liang"
      user-login-name "honnix"
      user-mail-address "hxliang1982@gmail.com")

;; scroll before we reach the end of the screen
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

;; y/n for short
(fset 'yes-or-no-p 'y-or-n-p)
;; =============================================================================
;; =============================================================================

;; =============================================================================
;; ======================== this is where packages start =======================
;; =============================================================================

;; Use clipboard when copy/paste in X
(use-package select
  :ensure t
  :disabled
  :init
  (setq select-enable-clipboard t))

;; appointment
(use-package appt
  :preface (provide 'appt)
  :disabled
  :config
  (appt-activate))

(use-package mule
  :preface (provide 'mule)
  :init
  (setq default-input-method 'MacOSX)
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package mule-cmds
  :preface (provide 'mule-cmds)
  :config
  (set-language-environment 'UTF-8))

(use-package server
  :ensure t
  :config
 (server-start))

(use-package ns-win
  :preface (provide 'ns-win)
  :init
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

(use-package hippie-expand
  :preface (provide 'hippie-expand)
  :bind ([(meta ?/)] . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package simple
  :preface (provide 'simple)
  :bind (("<end>" . move-end-of-line)
         ("<home>" . move-beginning-of-line))
  :init
  (setq-default kill-whole-line t
                column-number-mode t)
  :config
  ;; at most 120 charactors per line
  (set-fill-column 120))

(use-package files
  :preface (provide 'files)
  :init
  (setq-default make-backup-files nil)
  :config
  (setq frame-title-format
        '("" invocation-name ": "
          (:eval
           (if (buffer-file-name)
               (abbreviate-file-name (buffer-file-name))
             "%b@emacs")))))

(use-package mwheel
  :preface (provide 'mwheel)
  :config
  (mouse-wheel-mode 1))

(use-package mouse
  :preface (provide 'mouse)
  :init
  (setq mouse-yank-at-point t))

;; move up mouse when cursor comes
(use-package avoid
  :ensure t
  :config
  (mouse-avoidance-mode 'animate))

;; hide shell password
(use-package comint
  :preface (provide 'comint)
  :commands (comint-output-filter-functions comint-watch-for-password-prompt)
  :init
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt))

(use-package custom
  :preface (provide 'custom)
  :config
  ;; (add-to-list 'custom-theme-load-path
  ;;   (file-name-as-directory "~/.emacs.d/personal/themes"))
  ;; (load-theme 'monokai t t)
  ;; (enable-theme 'monokai)
  (load-theme 'zenburn t))

;; whenever there are more than one files with the same
;; name, use directory as prefix, not foobar<?>
(use-package uniquify
  :preface (provide 'uniquify)
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package indent
  :preface (provide 'indent)
  :config
  (use-package subr
    :preface (provide 'subr)
    :init
    (setq tab-stop-list (number-sequence 4 120 4))))

;; flash mode line as visiual indication
(use-package faces
  :preface (provide 'faces)
  :config
  (use-package timer
    :preface (provide 'timer)
    :config
    (setq visible-bell nil
          ring-bell-function (lambda ()
                               (invert-face 'mode-line)
                               (run-with-timer 0.1 nil 'invert-face 'mode-line)))))
(use-package tool-bar
  :preface (provide 'tool-bar)
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :preface (provide 'scroll-bar)
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :unless (eq system-type 'darwin)
  :preface (provide 'menu-bar)
  :config
  (menu-bar-mode -1))

;; display date and time
(use-package time
  :ensure t
  :init
  (setq display-time-day-and-date t
        display-time-24hr-format t
        display-time-use-mail-icon t
        display-time-interval 10)
  :config
  (display-time))

(use-package shell
  :ensure t
  :hook (shell-mode . my-shell-mode-hook-func)
  :config
  ;; kill shell buffer when shell exits
  (defun my-shell-mode-hook-func ()
    (set-process-sentinel (get-buffer-process (current-buffer))
                          'my-shell-mode-kill-buffer-on-exit))
  (defun my-shell-mode-kill-buffer-on-exit (process state)
    (message "%s" state)
    (if (or
         (string-match "exited abnormally with code.*" state)
         (string-match "finished" state))
        (kill-buffer (current-buffer)))))

(use-package linum
  :ensure t
  :config
  (global-linum-mode))

(use-package text-mode
  :preface (provide 'text-mode)
  :init
  (setq initial-major-mode 'text-mode
        major-mode 'text-mode))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings))

;; just show matching parenthesis, no jump
(use-package paren
  :ensure t
  :init
  (setq show-paren-style 'parentheses)
  :config
  (show-paren-mode t))

;; no separate frame when ediff
(use-package ediff
  :ensure t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; winner-mode to support undo/redo window layout
(use-package winner
  :ensure t
  :config
  (winner-mode 1))

(use-package startup
  :preface (provide 'startup)
  :init
  (setq initial-scratch-message ""
        inhibit-startup-message t))

(use-package isearch
  :preface (provide 'isearch)
  :bind (("C-*" . isearch-current-symbol)
         ("C-#" . isearch-backward-current-symbol))
  :config
  (defun isearch-yank-regexp (regexp)
    "Pull REGEXP into search regexp." 
    (let ((isearch-regexp nil)) ;; Dynamic binding of global.
      (isearch-yank-string regexp))
    (isearch-search-and-update))
  
  (defun isearch-yank-symbol (&optional partialp backward)
    "Put symbol at current point into search string.
    
    If PARTIALP is non-nil, find all partial matches."
    (interactive "P")
    
    (let (from to bound sym)
      (setq sym
            ;; this block taken directly from find-tag-default
            ;; we couldn't use the function because we need the internal from and to values
            (when (or (progn
                        ;; Look at text around `point'.
                        (save-excursion
                          (skip-syntax-backward "w_") (setq from (point)))
                        (save-excursion
                          (skip-syntax-forward "w_") (setq to (point)))
                        (> to from))
                      ;; Look between `line-beginning-position' and `point'.
                      (save-excursion
                        (and (setq bound (line-beginning-position))
                             (skip-syntax-backward "^w_" bound)
                             (> (setq to (point)) bound)
                             (skip-syntax-backward "w_")
                             (setq from (point))))
                      ;; Look between `point' and `line-end-position'.
                      (save-excursion
                        (and (setq bound (line-end-position))
                             (skip-syntax-forward "^w_" bound)
                             (< (setq from (point)) bound)
                             (skip-syntax-forward "w_")
                             (setq to (point)))))
              (buffer-substring-no-properties from to)))
      (cond ((null sym)
             (message "No symbol at point"))
            ((null backward)
             (goto-char (1+ from)))
            (t
             (goto-char (1- to))))
      (isearch-search)
      (if partialp
          (isearch-yank-string sym)
        (isearch-yank-regexp
         (concat "\\_<" (regexp-quote sym) "\\_>")))))

  (defun isearch-current-symbol (&optional partialp)
    "Incremental search forward with symbol under point.

    Prefixed with \\[universal-argument] will find all partial
    matches."
    (interactive "P")
    (let ((start (point)))
      (isearch-forward-regexp nil 1)
      (isearch-yank-symbol partialp)))

  (defun isearch-backward-current-symbol (&optional partialp)
    "Incremental search backward with symbol under point.

    Prefixed with \\[universal-argument] will find all partial
    matches."
    (interactive "P")
    (let ((start (point)))
      (isearch-backward-regexp nil 1)
      (isearch-yank-symbol partialp))))

(use-package ivy
  :ensure t
  :bind (("\C-s" . swiper-isearch)
         ("\C-r" . swiper-isearch-backward)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind (("\C-s" . swiper-isearch)
         ("\C-r" . swiper-isearch-backward)))

(use-package ibuffer
  :ensure t
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("<up>" . ibuffer-previous-line)
         ("<down" . ibuffer-next-line))
  :hook (ibuffer-mode . switch-to-default-group)
  :init
  ;; modify the default ibuffer-formats
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 50 50 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process))
        ibuffer-saved-filter-groups
        '(("default"
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*Compile-Log\\*$")
                           (name . "^\\*Completions\\*$")
                           (name . "^\\*Help\\*$")
                           (name . "^\\*Kill Ring\\*$")
                           (name . "^\\*Packages\\*$")
                           (mode . emacs-lisp-mode)))
                 ("dired" (mode . dired-mode))
                 ("yaml" (mode . yaml-mode))
                 ("vc" (or
                        (mode . git-commit-mode)
                        (mode . git-commit-major-mode)
                        (mode . git-rebase-mode)
                        (mode . magit-mode)
                        (mode . magit-cherry-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-log-mode)
                        (mode . magit-log-select-mode)
                        (mode . magit-merge-preview-mode)
                        (mode . magit-popup-mode)
                        (mode . magit-process-mode)
                        (mode . magit-refs-mode)
                        (mode . magit-reflog-mode)
                        (mode . magit-revision-mode)
                        (mode . magit-stash-mode)
                        (mode . magit-stashes-mode)
                        (mode . magit-status-mode)
                        (mode . diff-mode)))
                 ("scala" (mode . scala-mode))
                 ("ttl" (mode . n3-mode))
                 ("python" (mode . python-mode))
                 ("go" (mode . go-mode))
                 ("prolog" (mode . prolog-mode)))))
  :config
  (defun switch-to-default-group ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  ;; use human readable size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (defun ibuffer-previous-line ()
    (interactive) (previous-line)
    (if (<= (line-number-at-pos) 2)
        (goto-line (- (count-lines (point-min) (point-max)) 2))))
  (defun ibuffer-next-line ()
    (interactive) (next-line)
    (if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
        (goto-line 3)))
  (defadvice ibuffer-generate-filter-groups
      (after reverse-ibuffer-groups () activate)
    (setq ad-return-value (nreverse ad-return-value))))

(use-package session
  :ensure t
  :config
  (session-initialize))

(use-package ido
  :ensure t
  :config
  (ido-mode t))

(use-package dired-single
  :ensure t
  :bind (:map dired-mode-map
              ([return] . dired-single-buffer)
              ([mouse-1] . dired-single-buffer-mouse)
              ("^" . go-to-parent))
  :config
  (defun go-to-parent ()
    (interactive)
    (dired-single-buffer "..")))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package browse-kill-ring
  :ensure t
  :bind ("C-c k" . browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings))

(use-package iy-go-to-char
  :ensure t
  :bind (("C-c f" . iy-go-to-char)
         ("C-c F" . iy-go-to-char-backward)
         ("C-c ;" . iy-go-to-char-continue)
         ("C-c ," . iy-go-to-char-continue-backward)))

(use-package redo+
  :ensure t
  :bind ("C-?" . redo))

;; do not use Emacs built-in page down/up
(use-package pager
  :ensure t
  :bind (("C-v" . pager-page-down)
         ([next] . pager-page-down)
         ("M-v" . pager-page-up)
         ([prior] . pager-page-up)
         ([M-up] . pager-row-up)
         ([M-kp-8] . pager-row-up)
         ([M-down] . pager-row-down)
         ([M-kp-2] . pager-row-down)))

(use-package tramp
  :ensure t
  :init
  (setq tramp-default-method "ssh"))

(use-package table
  :ensure t
  :init
  (setq table-disable-advising t)
  :hook (text-mode . table-recognize))

(use-package magit
  :ensure t
  :bind ("C-x RET" . magit-status))

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay .3
        company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(use-package flyspell
  :ensure t
  :ensure-system-package aspell
  :init
  (setq ispell-program-name "aspell")
  :hook (text-mode . flyspell-mode))

(use-package plantuml-mode
  :ensure t
  :disabled
  :init
  (setq plantuml-jar-path
        "/usr/local/opt/plantuml/libexec/plantuml.jar"))

;; delete all whitespace up the first non-whitespace character
(use-package hungry-delete
  :ensure t
  :bind (("<M-backspace>" . hungry-delete-backward)
         ("<M-delete>" . hungry-delete-forward)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package centaur-tabs
  :ensure t
  :demand
  :after all-the-icons
  :bind (("<C-M-up>" . centaur-tabs-backward-group)
         ("<C-M-down>" . centaur-tabs-forward-group)
         ("<C-M-left>" . centaur-tabs-backward)
         ("<C-M-right>" . centaur-tabs-forward))
  :init
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        x-underline-at-descent-line t)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(use-package treemacs
  :ensure t
  :bind ("C-c t" . treemacs)
  :init
  (setq treemacs-no-png-images t))

(use-package vimish-fold
  :ensure t
  :bind (("C-c v f" . vimish-fold)
         ("C-c v v" . vimish-fold-delete))
  :config
  (vimish-fold-global-mode 1))

(use-package smart-mode-line
  :ensure t
  :after smart-mode-line-atom-one-dark-theme
  :init
  (setq sml/theme 'atom-one-dark)
  :config
  (sml/setup))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package multi-scratch
  :load-path "3rd"
  :bind ("C-c s" . multi-scratch-new))

(use-package compile
  :ensure t
  :init
  (setq compilation-window-height 8
        compilation-finish-functions
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "COMPILATION ERROR!, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (if (string-match "*compilation*" buf)
              (run-at-time 0.5 nil 'delete-windows-on buf))
          (message "no compilation error.")))))

(use-package projectile
  :ensure t
  :bind-keymap ("s-p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))

;; =============================================================================
;; ======================== the last things should happen=======================
;; =============================================================================
;; set frame size
(set-frame-height (selected-frame) 40)
(set-frame-width (selected-frame) 120)
