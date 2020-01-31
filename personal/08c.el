(use-package ctypes
  :ensure t
  :config
  (ctypes-auto-parse-mode 1))


(setq c-basic-offset 4)
(c-set-offset 'inline-open 0)
(c-set-offset 'inline-close 0)

(setq c-default-style (quote ((c-mode . "stroustrup")
                              (c++-mode . "stroustrup")
                              (java-mode . "java")
                              (other . "stroustrup"))))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (which-func-mode 1)
             (hide-ifdef-mode 1)))
