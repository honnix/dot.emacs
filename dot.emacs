(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("melpa" . "http://melpa.org/packages/")
                          ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                          ))
(package-initialize)

;============================load paths===================================
(add-to-list 'load-path (expand-file-name "~/.emacs.d/3rd"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal"))
;;=========================================================================

(mapc 'load
      (directory-files
       (expand-file-name "~/.emacs.d/personal") t "\\.el$"))
