;;; 26sh.el --- sh

;;; Commentary:

;;; Code:

(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zshrc\\'" . sh-mode)
         ("\\.zshenv\\'" . sh-mode)))

;;; 26sh.el ends here
