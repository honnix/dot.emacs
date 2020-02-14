;;; 06python.el.el --- python

;;; Commentary:

;;; Code:

(use-package elpy
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :bind ([remap python-shell-send-buffer] . comment-dwim)
  :init
  (setq python-indent-guess-indent-offset-verbose nil
		python-indent-offset 4)
  :config
  (elpy-enable))

;;; 06python.el ends here
