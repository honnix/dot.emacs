;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;============================load paths===================================
(add-to-list 'load-path (expand-file-name "~/.emacs.d/3rd"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal"))
;;=========================================================================

(mapc 'load
      (directory-files
       (expand-file-name "~/.emacs.d/personal") t "\\.el$"))
