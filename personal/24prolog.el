;;; 24prolog.el --- prolog -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package prolog
  :load-path "3rd"
  :commands (run-prolog prolog-mode)
  :mode ("\\.pl\\'" . prolog-mode)
  :init
  (setq prolog-system 'swi))

;;; 24prolog.el ends here
