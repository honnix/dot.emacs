;;; 24prolog.el --- prolog

;;; Commentary:

;;; Code:

(use-package prolog
  :load-path "3rd"
  :commands (run-prolog prolog-mode)
  :mode ("\\.pl\\'" . prolog-mode)
  :bind ("C-c C-c" . comment-dwim)
  :init
  (setq prolog-system 'swi)
  :config
  (unbind-key "C-c C-c" prolog-mode-map))

;;; 24prolog.el ends here
