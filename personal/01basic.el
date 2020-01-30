;; ============================= customize ============================
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;;=========================================================================

(setq mac-command-modifier 'meta)

(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<delete>") 'delete-char)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(setq locale-coding-system 'utf-8
      default-input-method 'MacOSX)

;; (setenv "PATH"
;;         (concat
;;          "~/bin" ":"
;;          "/usr/local/bin" ":"
;;          "~/Developer/go/bin" ":"
;;          (getenv "PATH")))

;; (setq exec-path (append '("~/bin"
;;                           "/usr/local/bin"
;;                           "~/Developer/go/bin") exec-path))

;; set font
;; (if (not (eq system-type 'darwin))
;;     (progn
;;       (set-frame-font "DejaVu Sans Mono-10")
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         'han '("WenQuanYi Zen Hei Mono" . "unicode-bmp"))))
 
;; Evaluate and replace the text
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "refreshed open files"))

;; move to window easily
(windmove-default-keybindings)

;; jump between two points
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
;;   (setq zmacs-region-stays t)
  (point-to-register 8))
(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
;;   (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))
(global-set-key (kbd "C-.") 'ska-point-to-register)
(global-set-key (kbd "C-,") 'ska-jump-to-register)

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
  (forward-line -1)
  )
(global-set-key (kbd "C-o") 'my-newline)
(global-set-key (kbd "C-S-o") 'my-newline-pre)

(global-font-lock-mode t)
(global-linum-mode)

(setq visible-bell nil
      ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq inhibit-startup-message t
      column-number-mode t
      mouse-yank-at-point t)

;; no indent by tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4
      tab-stop-list (number-sequence 4 120 4))

;; for Chinese punctuation
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; major mode
(setq initial-major-mode 'text-mode
      major-mode 'text-mode)

;; user information
(setq user-full-name "Hongxin Liang"
      user-login-name "honnix"
      user-mail-address "hxliang1982@gmail.com")

;; just show matching parenthesis, no jump
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; Show buffer name
(setq frame-title-format
      '("" invocation-name ": "
        (:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b@emacs"))))

;; mark with Shift+direction
;(pc-selection-mode)

;; display date and time
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-use-mail-icon t
      display-time-interval 10)
(display-time)

;; scroll before we reach the end of the screen
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

;; start 'emacs server'
(server-start)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; no menu unless it is macOSX
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

;; speed up moving through VERY large file
;(setq lazy-lock-defer-on-scrolling t)
;(setq font-lock-support-mode 'lazy-lock-mode)
;(setq font-lock-maximum-decoration t)

;; How to deel with the errors in .emacs file
;; (condition-case err
;; 	(progn
;; 	  (require 'xxx))
;;   (error
;;    (message "Can't load xxx-mode %s" (cdr err))))

;; no backup files
(setq-default make-backup-files nil)

;; drive the wheel mouse
(mouse-wheel-mode 1)
;; move up mouse when cursor comes
(mouse-avoidance-mode 'animate)

;; Use clipboard when copy/paste in X
;; (setq x-select-enable-clipboard t)

;; whenever there are more than one files with the same
;; name, use directory as prefix, not foobar<?>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; y/n for short
(fset 'yes-or-no-p 'y-or-n-p)

;; at most 120 charactors per line
(set-fill-column 120)

;; other global set keys
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key (kbd "C-c g") 'goto-line)

;; which shell to use
(setq shell-file-name "/usr/local/bin/zsh")

;; hide shell password
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; auto complete
(global-set-key [(meta ?/)] 'hippie-expand)
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
        try-complete-lisp-symbol))

;; kill shell buffer when shell exits
(add-hook 'shell-mode-hook 'my-shell-mode-hook-func)
(defun my-shell-mode-hook-func ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        'my-shell-mode-kill-buffer-on-exit))
(defun my-shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;; auto mode list
;; (mapc
;;  (function (lambda (setting)
;; 	     (setq auto-mode-alist
;; 		   (cons setting auto-mode-alist))))
;;  '(("\\.xml$" .  sgml-mode)
;;    ("\\\.bash" . sh-mode)
;;    ("\\.rdf$" .  sgml-mode)
;;    ("\\.session" . emacs-lisp-mode)
;;    ("\\.l$" . c-mode)
;;    ("\\.h$" . c++-mode)
;;    ("\\.css$" . css-mode)
;;    ("\\.cfm$" . html-mode)
;;    ("gnus" . emacs-lisp-mode)
;;    ("\\.idl$" . idl-mode)))

;; kill whole line
(setq-default kill-whole-line t)

;; set EOL style
;; (setq inhibit-eol-conversion 'gbk-dos)

;; select current line
(global-set-key (kbd "C-c l")
                '(lambda ()
                   (interactive)
                   (move-beginning-of-line nil)
                   (set-mark-command nil)
                   (move-end-of-line nil)))

;; appointment
;; (appt-activate)

(require 'multi-scratch)
(global-set-key (kbd "C-c s") 'multi-scratch-new)

;; no separate frame when ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; winner-mode to support undo/redo window layout
(winner-mode 1)

;; Highlight symbol
(global-set-key (kbd "C-c h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c n") 'highlight-symbol-next)
(global-set-key (kbd "C-c p") 'highlight-symbol-prev)

;; (add-to-list 'custom-theme-load-path
;;   (file-name-as-directory "~/.emacs.d/personal/themes"))
;; (load-theme 'monokai t t)
;; (enable-theme 'monokai)
(load-theme 'zenburn t)

;; customize moving
;; (global-set-key (kbd "C-a") 'beginning-of-line)
;; (global-set-key (kbd "C-e") 'end-of-line)

(setq initial-scratch-message "")

;; VIM for good
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
    (isearch-yank-symbol partialp)))

(global-set-key (kbd "C-*") 'isearch-current-symbol)
(global-set-key (kbd "C-#") 'isearch-backward-current-symbol)

;; set frame size
(set-frame-height (selected-frame) 40)
(set-frame-width (selected-frame) 120)

;; ======================================================================
;; ==================== this is where packages start ====================
;; ======================================================================

(require 'use-package)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  (ivy-mode 1))

(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer)
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
        (quote (("default"
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
                 ("magit" (name . "^\\*magit.*$"))
                 ("scala" (mode . scala-mode))
                 ("ttl" (mode . n3-mode))
                 ("python" (mode . python-mode))
                 ("go" (mode . go-mode))
                 ("prolog" (mode . prolog-mode))))))
  :config
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
  (define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
  (define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                   activate)
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
  :config
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

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
  :bind ("C-?"  . redo))

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
  :hook (text-mode table-recognize))

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
  :hook (text-mode flyspell-mode))

(use-package plantuml-mode
  :ensure t
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
        dashboard-items '((recents  . 5)))
  :config
  (dashboard-setup-startup-hook))
