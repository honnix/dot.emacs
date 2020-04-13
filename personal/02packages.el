;;; 02packages.el --- configure all packages

;;; Commentary:

;;; Code:

;; Use clipboard when copy/paste in X
(use-package select
  :ensure nil
  :disabled
  :init
  (setq select-enable-clipboard t))

(use-package bindings
  :preface (provide 'bindings)
  :bind ("M-o" . mode-line-other-buffer))

;; appointment
(use-package appt
  :ensure nil
  :disabled
  :config
  (appt-activate))

(use-package mule
  :ensure nil
  :init
  (setq default-input-method 'MacOSX)
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package mule-cmds
  :preface (provide 'mule-cmds)
  :demand
  :config
  (set-language-environment 'UTF-8))

(use-package ns-win
  :ensure nil
  :init
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand)
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
  :ensure nil
  :bind (("<end>" . move-end-of-line)
         ("<home>" . move-beginning-of-line))
  :init
  (setq-default kill-whole-line t
                column-number-mode t)
  :config
  (unbind-key "M-y"))

(use-package files
  :ensure nil
  :init
  (setq-default make-backup-files nil
                require-final-newline t)
  :config
  (setq frame-title-format
        '("" invocation-name ": "
          (:eval
           (if (buffer-file-name)
               (abbreviate-file-name (buffer-file-name))
             "%b@emacs")))))

(use-package mwheel
  :ensure nil
  :config
  (mouse-wheel-mode 1))

(use-package mouse
  :ensure nil
  :init
  (setq mouse-yank-at-point t))

;; move up mouse when cursor comes
(use-package avoid
  :ensure t
  :config
  (mouse-avoidance-mode 'animate))

;; hide shell password
(use-package comint
  :ensure nil
  :commands (comint-output-filter-functions comint-watch-for-password-prompt)
  :init
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt))

;; (use-package custom
;;   :ensure nil
;;   :config
;;   ;; (add-to-list 'custom-theme-load-path
;;   ;;   (file-name-as-directory "~/.emacs.d/personal/themes"))
;;   ;; (load-theme 'monokai t t)
;;   ;; (enable-theme 'monokai)
;;   (load-theme 'zenburn t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  (use-package doom-themes-ext-treemacs
    :init
    (setq doom-themes-treemacs-theme "doom-colors")) ; use the colorful treemacs theme)
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :demand
  :init
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-vcs-max-length 20)
  :config
  (doom-modeline-mode 1))

;; whenever there are more than one files with the same
;; name, use directory as prefix, not foobar<?>
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package indent
  :preface (provide 'indent)
  :config
  (use-package subr
    :preface (provide 'subr)
    :init
    (setq tab-stop-list (number-sequence 4 120 4))))

;; (use-package faces
;;   :ensure nil
;;   :config
;;   ;; zenburn region background is a bit hard to see
;;   (set-face-attribute 'region nil :background "gray13"))

;; (use-package timer
;;   :ensure nil
;;   :after faces
;;   :config
;;   (setq visible-bell nil
;;         ring-bell-function (lambda ()
;;                              (invert-face 'mode-line)
;;                              (run-with-timer 0.1 nil 'invert-face 'mode-line))))

(use-package hl-line
  :after faces
  :config
  (global-hl-line-mode 1)
  ;; make it work better with zenburn
  ;;(set-face-attribute 'hl-line nil :background "gray32"))
  )

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :unless (eq system-type 'darwin)
  :ensure nil
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

(use-package display-line-numbers
  :ensure t
  :config
  (global-display-line-numbers-mode))

(use-package display-fill-column-indicator
  :ensure t
  :commands (display-fill-column-indicator-mode))

(use-package text-mode
  :ensure nil
  :hook ((text-mode . auto-fill-mode)
         (text-mode . display-fill-column-indicator-mode))
  :init
  (setq initial-major-mode 'text-mode
        major-mode 'text-mode))

(use-package highlight-indent-guides
  :ensure t
  :delight highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . highlight-indent-guides-mode)
         (prog-mode . display-fill-column-indicator-mode)))

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
  :ensure nil
  :commands (statup)
  :init
  (setq initial-scratch-message ""
        inhibit-startup-message t))

(use-package isearch
  :ensure nil
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
  :delight ivy-mode
  :demand
  :bind (("C-c C-r" . ivy-resume))
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

(use-package counsel
  :ensure t
  :delight counsel-mode
  :demand
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop))
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
         ("<up>" . ibuffer-backward-line)
         ("<down>" . ibuffer-forward-line))
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
                filename-and-process)))
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
  ;; (defun ibuffer-previous-line ()
  ;;   (interactive) (previous-line)
  ;;   (if (<= (line-number-at-pos) 2)
  ;;       (goto-line (- (count-lines (point-min) (point-max)) 2))))
  ;; (defun ibuffer-next-line ()
  ;;   (interactive) (next-line)
  ;;   (if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
  ;;       (goto-line 3)))
  )

(use-package ibuf-ext
  :init
  (setq ibuffer-saved-filter-groups
        '(("default"
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
           ("lsp" (or
                   (name . "^\\*LSP.*\\*$")
                   (name . "^\\*lsp.*\\*$")
                   (name . "^\\*gopls.*\\*$")
                   (name . "^\\*pyls.*\\*$")))
           ("emacs" (or
                     (name . "^\\*dashboard\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*Compile-Log\\*$")
                     (name . "^\\*Completions\\*$")
                     (name . "^\\*Help\\*$")
                     (name . "^\\*Kill Ring\\*$")
                     (name . "^\\*Packages\\*$")
                     (mode . emacs-lisp-mode)))
           ("scratch" (or
                       (name . "^\\*scratch\\*$")
                       (name . "^\\*multi-scratch.*\\*$")))
           ("java" (mode . java-mode))
           ("scala" (mode . scala-mode))
           ("ttl" (mode . n3-mode))
           ("js" (or
                  (mode . js-mode)
                  (mode . js2-mode)))
           ("prolog" (mode . prolog-mode))
           ("markdown" (mode . markdown-mode))
           ("python" (mode . python-mode))
           ("go" (mode . go-mode)))))
  :config
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

(use-package browse-kill-ring
  :ensure t
  :bind ("C-c k" . browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings))

(use-package avy
  :ensure t
  :commands (avy-process)
  :bind (("C-c f" . avy-goto-char-timer)
         ("C-c g" . avy-goto-line)
         ("C-c C-j" . avy-resume)))

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
         ([M-kp-8] . pager-row-up)
         ([M-kp-2] . pager-row-down)))

(use-package tramp
  :ensure t
  :init
  (setq tramp-default-method "ssh"))

(use-package table
  :ensure t
  :init
  ;; (setq table-disable-advising t)
  :hook (text-mode . table-recognize))

(use-package magit
  :ensure t
  :bind ("C-x RET" . magit-status))

(use-package autopair
  :ensure t
  :delight autopair-mode
  :config
  (autopair-global-mode))

(use-package yasnippet
  :ensure t
  :delight yas-global-mode
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
  :demand t
  :init
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p". company-select-previous))
  :config
  (global-company-mode))

(use-package company-dabbrev
  :ensure nil
  :init
  (setq company-dabbrev-downcase nil))

(use-package flyspell
  :ensure t
  :ensure-system-package aspell
  :init
  (setq ispell-program-name "aspell")
  :hook (text-mode . flyspell-mode)
  :config
  (unbind-key "C-;" flyspell-mode-map))

(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path "/usr/local/bin/plantuml.jar"
        plantuml-default-exec-mode 'jar
        plantuml-output-type "txt"))

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
  :delight drag-stuff-mode
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package all-the-icons
  :ensure t)

(use-package centaur-tabs
  :ensure t
  :demand
  :requires all-the-icons
  :bind (("<C-M-up>" . centaur-tabs-backward-group)
         ("<C-M-down>" . centaur-tabs-forward-group)
         ("<C-M-left>" . centaur-tabs-backward)
         ("<C-M-right>" . centaur-tabs-forward))
  :init
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "\xf111"
        x-underline-at-descent-line t)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project))

(use-package treemacs
  :ensure t
  :bind ("C-c t" . treemacs)
  :init
  (setq treemacs-indentation 1)
  :config
  (treemacs-resize-icons 15))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package vimish-fold
  :ensure t
  :bind (("C-c v f" . vimish-fold)
         ("C-c v v" . vimish-fold-delete))
  :config
  (vimish-fold-global-mode 1))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t
        dashboard-startup-banner 2
        dashboard-set-file-icons t
        dashboard-items '((recents  . 20)
                          (projects . 20)
                          (bookmarks . 20)))
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
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind-keymap ("s-p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :bind (:map projectile-command-map
              ("SPC" . counsel-projectile)))

(use-package expand-region
  :ensure t
  :bind (("C-M-w" . er/expand-region)
         ("C-M--" . er/contract-region)))

(use-package delsel
  :ensure t
  :config
  (pending-delete-mode t))

(use-package insert-shebang
  :ensure t)

(use-package lsp-mode
  :ensure t
  ;; uncomment to enable gopls http debug server
  ;; :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:3000"))
  :init
  (setq lsp-idle-delay 0.500)
  :commands (lsp lsp-deferred)
  :config
  ;; zenburn region background is a bit hard to see
  ;; (set-face-attribute 'lsp-face-highlight-textual nil :background "gray40"))
  )

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-ui
  :ensure t
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-M-d" . lsp-ui-doc-show))
  :init
  (setq lsp-ui-peek-fontify 'always
        ;; lsp-ui-doc-position 'top
        lsp-ui-doc-enable nil))

(use-package autorevert
  :ensure t
  :delight auto-revert-mode
  :init
  (setq auto-revert-check-vc-info t))

(use-package git-gutter
  :ensure t
  :delight git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :ensure t)

(use-package server
  :ensure t
  :config
  (server-start))

;;; 02packages.el ends here
