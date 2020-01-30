(use-package n3-mode
  :ensure t
  :defer t
  :init
  (autoload 'n3-mode "n3-mode" nil t)
  (setq auto-mode-alist
      (append
       (list
        '("\\.n3" . n3-mode)
        '("\\.ttl" . n3-mode)
        '("\\.owl" . n3-mode))
       auto-mode-alist))
  :hook (turn-on-font-lock))

