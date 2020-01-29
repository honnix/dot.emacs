;; ============================= customize ============================
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;;=========================================================================

(setq mac-command-modifier 'meta)

(setenv "PATH"
        (concat
         "~/bin" ":"
         "/usr/local/bin" ":"
         "~/Developer/go/bin" ":"
         (getenv "PATH")))

(setq exec-path (append '("~/bin"
                          "/usr/local/bin"
                          "~/Developer/go/bin") exec-path))

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("melpa" . "http://melpa.org/packages/")
                          ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                          ))
(package-initialize)

;; Set font
;; (if (not (eq system-type 'darwin))
;;     (progn
;;       (set-frame-font "DejaVu Sans Mono-10")
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         'han '("WenQuanYi Zen Hei Mono" . "unicode-bmp"))))

;; Emacs tabbar
;; (if (display-graphic-p)
;;     (progn
;;       (tabbar-mode)
;;       (global-set-key (kbd "<C-M-up>") 'tabbar-backward-group)
;;       (global-set-key (kbd "<C-M-down>") 'tabbar-forward-group)
;;       (global-set-key (kbd "<C-M-left>") 'tabbar-backward)
;;       (global-set-key (kbd "<C-M-right>") 'tabbar-forward)
;;       (setq tabbar-background-color "#959A79")
;;       (custom-set-faces
;;        '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
;;        '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
;;        '(tabbar-button-highlight ((t (:inherit tabbar-default))))
;;        '(tabbar-highlight ((t (:underline t))))
;;        '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
;;        '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59"))))
;;        '(tabbar-unselected ((t (:inherit tabbar-default)))))))

;; Move to window easily
(windmove-default-keybindings)

;; Simply ivy configuration
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper-isearch)
(global-set-key "\C-r" 'swiper-isearch-backward)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Use ibuffer as default buffer list processor
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 50 50 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))

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

(setq ibuffer-saved-filter-groups
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

(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                 activate)
  (setq ad-return-value (nreverse ad-return-value)))

;; Load session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; key mapping for mac
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<delete>") 'delete-char)

;; Load ido
(require 'ido)
(ido-mode t)

;; Use a single buffer for dired mode
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
        loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Highlight current editing line
;; (require 'hl-line)
;; (global-hl-line-mode 1)

;; Browse kill ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Jump between two points
(global-set-key (kbd "C-.") 'ska-point-to-register)
(global-set-key (kbd "C-,") 'ska-jump-to-register)
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

(require 'iy-go-to-char)
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)

;; Setup UTF-8 environment
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(setq locale-coding-system 'utf-8)
(setq default-input-method 'MacOSX)

(defun my-newline ()
  "New line after current line."
  (interactive)
  (move-end-of-line 1)
  (newline)
  )
(global-set-key (kbd "C-o") 'my-newline)

(defun my-newline-pre ()
  "New line before current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1)
  )
(global-set-key (kbd "C-S-o") 'my-newline-pre)

;; Hilight
(global-font-lock-mode t)

;; Line numbers
(global-linum-mode)

;; No beep, visible bell instead
;; (setq visible-bell t)

;; No startup message
(setq inhibit-startup-message t)

;; We need column number too
(setq column-number-mode t)

;; Yank at cursor point
(setq mouse-yank-at-point t)

;; No indent by tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

;; For Chinese punctuation
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Major mode
(setq initial-major-mode 'text-mode)
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Screen lines mode
(autoload 'screen-lines-mode "screen-lines"
  "Toggle Screen Lines minor mode for the current buffer." t)
(autoload 'turn-on-screen-lines-mode "screen-lines"
  "Turn on Screen Lines minor mode for the current buffer." t)
(autoload 'turn-off-screen-lines-mode "screen-lines"
  "Turn off Screen Lines minor mode for the current buffer." t)
(add-hook 'text-mode-hook 'turn-on-screen-lines-mode)

;; User information
(setq user-full-name "Hongxin Liang")
(setq user-mail-address "hxliang1982@gmail.com")

;; Just show, no jump
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; Show buffer name
(setq frame-title-format
      '("" invocation-name ": "
        (:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b@emacs"))))

;; Mark with Shift+direction
;(pc-selection-mode)

;; Whenever there are more than one files with the same
;; name, use directory as prefix, not foobar<?>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; y/n for short
(fset 'yes-or-no-p 'y-or-n-p)

;; Display date and time
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-use-mail-icon t)
(setq display-time-interval 10)
(display-time)

;; Scroll before we reach the end of the screen
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

;; Start 'emacs server'
(server-start)

;; No toolbar
(tool-bar-mode -1)

;; No menu
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

;; No scroll bar
(scroll-bar-mode -1)

;; Speed up moving through VERY large file
;(setq lazy-lock-defer-on-scrolling t)
;(setq font-lock-support-mode 'lazy-lock-mode)
;(setq font-lock-maximum-decoration t)

;; How to deel with the errors in .emacs file
;; (condition-case err
;; 	(progn
;; 	  (require 'xxx))
;;   (error
;;    (message "Can't load xxx-mode %s" (cdr err))))

;; No backup files
(setq-default make-backup-files nil)

;; Drive the wheel mouse
(mouse-wheel-mode 1)

;; Use clipboard when copy/paste in X
;; (setq x-select-enable-clipboard t)

;; Move up mouse when cursor comes
(mouse-avoidance-mode 'animate)

;; Redo support
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

;; At most 120 charactors per line
(set-fill-column 120)

;; Do not use Emacs built-in page down/up
(require 'pager)
(global-set-key (kbd "C-v") 'pager-page-down)
(global-set-key [next] 'pager-page-down)
(global-set-key (kbd "M-v") 'pager-page-up)
(global-set-key [prior] 'pager-page-up)
(global-set-key '[M-up] 'pager-row-up)
(global-set-key '[M-kp-8] 'pager-row-up)
(global-set-key '[M-down] 'pager-row-down)
(global-set-key '[M-kp-2] 'pager-row-down)

;; Other global set keys
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key (kbd "C-c g") 'goto-line)

;; Which shell to use
(setq shell-file-name "/usr/local/bin/zsh")

;; Hide shell password
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Auto complete
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

(require 'tramp)
(setq tramp-default-method "ssh")

;; Kill shell buffer when shell exits
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

;; Auto mode list
(mapc
 (function (lambda (setting)
	     (setq auto-mode-alist
		   (cons setting auto-mode-alist))))
 '(("\\.xml$" .  sgml-mode)
   ("\\\.bash" . sh-mode)
   ("\\.rdf$" .  sgml-mode)
   ("\\.session" . emacs-lisp-mode)
   ("\\.l$" . c-mode)
   ("\\.h$" . c++-mode)
   ("\\.css$" . css-mode)
   ("\\.cfm$" . html-mode)
   ("gnus" . emacs-lisp-mode)
   ("\\.idl$" . idl-mode)))

;; Kill whole line
(setq-default kill-whole-line t)

;; Set EOL style
;; (setq inhibit-eol-conversion 'gbk-dos)

;; Select current line
(global-set-key (kbd "C-c l")
                '(lambda ()
                   (interactive)
                   (move-beginning-of-line nil)
                   (set-mark-command nil)
                   (move-end-of-line nil)))

;; Delete line without kill-ring effect
(global-set-key (kbd "C-S-k")
                '(lambda ()
                   (interactive)
                   (delete-region (point)
                                  (progn (forward-line 1) (point)))))

;; Insert table, after waiting for a long time, Emacs
;; finally fixed this bug
(setq table-disable-advising t)
(require 'table)
(add-to-list 'text-mode-hook 'table-recognize)

;; Appointment
(appt-activate)

;; No separate frame when ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; winner-mode to support undo/redo window layout
(winner-mode 1)

;; Highlight symbol
(global-set-key (kbd "C-c h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c n") 'highlight-symbol-next)
(global-set-key (kbd "C-c p") 'highlight-symbol-prev)

;; Set colors
;; (add-to-list 'custom-theme-load-path
;;   (file-name-as-directory "~/.emacs.d/personal/themes"))
;; (load-theme 'monokai t t)
;; (enable-theme 'monokai)
(load-theme 'zenburn t)

;; Customize moving
(global-set-key (kbd "C-a") 'beginning-of-line)
(global-set-key (kbd "C-e") 'end-of-line)

;; Empty initial scratch buffer
(setq initial-scratch-message "")

;; ;; Switch to *scratch* buffer quickly
;; (defun switch-buffer-scratch ()
;;   "Switch to the scratch buffer. If the buffer doesn't exist,
;; create it and write the initial message into it."
;;   (interactive)
;;   (let* ((scratch-buffer-name "*scratch*")
;;          (scratch-buffer (get-buffer scratch-buffer-name)))
;;     (unless scratch-buffer
;;       (setq scratch-buffer (get-buffer-create scratch-buffer-name))
;;       (with-current-buffer scratch-buffer
;;         (text-mode)
;;         (insert initial-scratch-message)))
;;     (switch-to-buffer scratch-buffer)))
;; (global-set-key (kbd "C-c s") 'switch-buffer-scratch)

;; Multiple scratches
(require 'multi-scratch)
(global-set-key (kbd "C-c s") 'multi-scratch-new)

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

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

;; magit
(global-set-key (kbd "C-x RET") 'magit-status)

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; Yasnippet to replace msf-abbrev which is rather dead
(require 'yasnippet)
(yas-global-mode 1)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay .3)
(setq company-dabbrev-downcase nil)

;; Spell check
(setq ispell-program-name "aspell")
(eval-after-load "flyspell"
  (lambda ()
     (define-key flyspell-mode-map (kbd "C-;") nil)
     (define-key flyspell-mode-map (kbd "C-.") nil)))

;; Set frame size
;; keep this at the end of the file!
(set-frame-height (selected-frame) 40)
(set-frame-width (selected-frame) 120)

;; plantuml jar path
(setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")

;; Delete all whitespace up the first non-whitespace character
(global-set-key (kbd "<M-backspace>") 'hungry-delete-backward)
(global-set-key (kbd "<M-delete>") 'hungry-delete-forward)


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Evaluate and replace the text
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

(global-flycheck-mode)

(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; TUI over GUI
(setq treemacs-no-png-images t)

;; centaur tabs
(require 'all-the-icons)
(require 'centaur-tabs)
(global-set-key (kbd "<C-M-up>")  'centaur-tabs-backward-group)
(global-set-key (kbd "<C-M-down>")  'centaur-tabs-forward-group)
(global-set-key (kbd "<C-M-left>")  'centaur-tabs-backward)
(global-set-key (kbd "<C-M-right>") 'centaur-tabs-forward)
(setq centaur-tabs-style "bar"
      centaur-tabs-set-bar 'under
      centaur-tabs-set-icons t
      x-underline-at-descent-line t)
(centaur-tabs-headline-match)
(centaur-tabs-mode t)

;; treemacs
(global-set-key (kbd "C-c t")  'treemacs)

;; fold
(vimish-fold-global-mode 1)
