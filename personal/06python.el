;;; 06python.el.el --- python

;;; Commentary:

;;; Code:

(use-package elpy
  :ensure t
  :init
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-offset 4)
  :config
  (elpy-enable)
  (unbind-key "C-c C-c" python-mode-map)
  (unbind-key "C-c C-c" elpy-mode-map))

;;; 06python.el ends here
