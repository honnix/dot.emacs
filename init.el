;;; init.el --- entrypoint

;;; Commentary:

;;; Code:

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ))
(package-initialize)

(mapc 'load
      (directory-files
       (expand-file-name "~/.emacs.d/personal") t "\\.el$"))

;;; init.el ends here
