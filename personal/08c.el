(require 'ctypes)
(ctypes-auto-parse-mode 1)

;; Set compilation window height
(setq compilation-window-height 8)

;; Reset some indentation offsets
(c-set-offset 'inline-open 0)
(c-set-offset 'inline-close 0)

;; C hook
;;; which-func-mode -- display in which function the current line is
(add-hook 'c-mode-common-hook
          '(lambda ()
             (which-func-mode 1)
             (hide-ifdef-mode 1)))
             ;; (my-c-mode-common-key-bind)))

;; (defun my-c-mode-common-key-bind ()
;;   (interactive)
;;   (local-unset-key (kbd "C-c C-c"))
;;   (local-set-key [f7] 'gdb)
;;   (local-set-key [f8] 'compile)
;;   (local-set-key [f6] 'ff-find-other-file)
;;   (local-set-key (kbd ".")
;;                  '(lambda ()
;;                     (interactive)
;;                     (insert ".")
;;                     (semantic-complete-analyze-inline)))
  ;; (local-set-key (kbd "C-c i /")
  ;;                '(lambda ()
  ;;                   (interactive)
  ;;                   (insert "/*\n* ")
  ;;                   (c-indent-command)
  ;;                   (insert "\n*/")
  ;;                   (c-indent-command)
  ;;                   (previous-line 1)
  ;;                   (back-to-indentation)
  ;;                   (forward-char 2)))
  ;; (local-set-key (kbd "C-c h d")
  ;;                '(lambda ()
  ;;                   (interactive)
  ;;                   (insert "#ifndef _H\n#define _H\n\n#endif /* _H */")
  ;;                   (previous-line 3)
  ;;                   (back-to-indentation)
  ;;                   (forward-char 8)))
;;   (local-set-key (kbd "C-c i m")
;;                  '(lambda ()
;;                     (interactive)
;;                     (insert "int main(int argc, char* argv[])\n{\n\nreturn 0;")
;;                     (c-indent-command)
;;                     (insert "\n}\n")
;;                     (previous-line 3)
;;                     (c-indent-command)))
;;   (local-set-key (kbd "C-c i c")
;;                  '(lambda ()
;;                     (interactive)
;;                     (insert "class \n{")
;;                     (c-indent-command)
;;                     (insert "\n\n};")
;;                     (c-indent-command)
;;                     (insert "\n")
;;                     (previous-line 4)
;;                     (back-to-indentation)
;;                     (forward-char 6)))
;;   (local-set-key (kbd "C-c i w")
;;                  '(lambda ()
;;                     (interactive)
;;                     (insert "while ()\n{")
;;                     (c-indent-command)
;;                     (insert "\n\n}")
;;                     (c-indent-command)
;;                     (previous-line 4)
;;                     (back-to-indentation)
;;                     (forward-char 7)))
;;   (local-set-key (kbd "C-c i f")
;;                  '(lambda ()
;;                     (interactive)
;;                     (insert "for ()\n{")
;;                     (c-indent-command)
;;                     (insert "\n\n}")
;;                     (c-indent-command)
;;                     (previous-line 4)
;;                     (back-to-indentation)
;;                     (forward-char 5)))
  ;; (local-set-key (kbd "C-c i i")
  ;;                '(lambda ()
  ;;                   (interactive)
  ;;                   (insert "int i = 0; i < ; ++i")
  ;;                   (backward-char 5)))
  ;; (local-set-key (kbd "C-c i j")
  ;;                '(lambda ()
  ;;                   (interactive)
  ;;                   (insert "int j = 0; j < ; ++j")
  ;;                   (backward-char 5)))
  ;; (local-set-key (kbd "RET")
  ;;                '(lambda ()
  ;;                   (interactive)
  ;;                   (if (looking-back "^ */?\\*[^/].*")
  ;;                       (progn
  ;;                         (insert "\n* ")
  ;;                         (c-indent-command))
  ;;                     (insert "\n")))))

;(setq c++-mode-abbrev-table nil)

;; ;; Generate main
;; (define-skeleton skeleton-c-mode-main-func
;;   "generate int main(int argc, char* argv[]) automatic" nil
;;   "int main(int argc, char* argv[])\n"
;;   "{" > "\n" 
;;   > _ "\n"
;;   "return 0;" > "\n" 
;;   "}" > "\n")
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("mains" "" skeleton-c-mode-main-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("mains" "" skeleton-c-mode-main-func 0)))

;; ;; Generate class
;; (define-skeleton skeleton-c-mode-class-func
;;   "generate class \n{\n\n};\n automatic" nil
;;   "class " _ "\n"
;;   "{" > "\n" 
;;   "\n"
;;   "};" > "\n")
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("classs" "" skeleton-c-mode-class-func 0)))

;; ;; Generate struct
;; (define-skeleton skeleton-c-mode-struct-func
;;   "generate struct \n{\n\n};\n automatic" nil
;;   "struct " _ "\n"
;;   "{" > "\n" 
;;   "\n"
;;   "};" > "\n")
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("structs" "" skeleton-c-mode-struct-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("structs" "" skeleton-c-mode-struct-func 0)))

;; ;; Generate namespace
;; (define-skeleton skeleton-c-mode-namespace-func
;;   "generate namespace \n{\n\n}\n automatic" nil
;;   "namespace " _ "\n"
;;   "{" > "\n"
;;   > @ "\n"
;;   "}" > "\n")
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("namespaces" "" skeleton-c-mode-namespace-func 0)))

;; ;; Generate for
;; (define-skeleton skeleton-c-mode-for-func
;;   "generate for ()\n{\n\n} automatic" nil
;;   "for (" _ ")\n"
;;   "{" > "\n"
;;   > @ "\n"
;;   "}" >)
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("fors" "" skeleton-c-mode-for-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("fors" "" skeleton-c-mode-for-func 0)))

;; ;; Generate for_each
;; (define-skeleton skeleton-c-mode-foreach-func
;;   "generate for_each(); automatic" nil
;;   "for_each (" _ ");"
;;   )
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("fores" "" skeleton-c-mode-foreach-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("fores" "" skeleton-c-mode-foreach-func 0)))

;; ;; Generate while
;; (define-skeleton skeleton-c-mode-while-func
;;   "generate while ()\n{\n\n} automatic" nil
;;   "while (" _ ")\n"
;;   "{" > "\n"
;;   > @ "\n"
;;   "}" >)
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("whiles" "" skeleton-c-mode-while-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("whiles" "" skeleton-c-mode-while-func 0)))

;; ;; Generate do ... while
;; (define-skeleton skeleton-c-mode-do-func
;;   "generate do\n{\n\n} while (); automatic" nil
;;   > "do\n"
;;   > "{" > "\n"
;;   > @ "\n"
;;   > "}" > " while (" _ ");")
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("dos" "" skeleton-c-mode-do-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("dos" "" skeleton-c-mode-do-func 0)))

;; ;; Generate if
;; (define-skeleton skeleton-c-mode-if-func
;;   "generate if ()\n{\n\n} automatic" nil
;;   > "if (" _ ")\n"
;;   > "{" > "\n"
;;   > @ "\n"
;;   > "}" >)
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("if" "" skeleton-c-mode-if-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("if" "" skeleton-c-mode-if-func 0)))

;; ;; Generate else
;; (define-skeleton skeleton-c-mode-else-func
;;   "generate else\n{\n\n} automatic" nil
;;   "else\n"
;;   "{" > "\n" 
;;   > _ "\n"
;;   "}" >)
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("elses" "" skeleton-c-mode-else-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("elses" "" skeleton-c-mode-else-func 0)))

;; ;; Generate else if
;; (define-skeleton skeleton-c-mode-else-if-func
;;   "generate else if ()\n{\n\n} automatic" nil
;;   "else if(" _ ")\n"
;;   "{" > "\n" 
;;   > @ "\n"
;;   "}" >)
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("elifs" "" skeleton-c-mode-else-if-func 0)))
;; (define-abbrev-table 'c-mode-abbrev-table
;;   '(("elifs" "" skeleton-c-mode-else-if-func 0)))

;; Generate include lib
(define-skeleton skeleton-c-mode-incl-func
  "generate #include <> automatic" nil
  "#include <"
  (completing-read "Include File:"
                   (mapcar '(lambda (f) (list f ))
                           (apply 'append
                                  (mapcar
                                   '(lambda (dir)
                                      (directory-files dir))
                                   (list "/usr/include"
                                         "/usr/lib/gcc/i686-pc-linux-gnu/4.1.2/include/g++-v4")))))
  ">")
(define-abbrev-table 'c++-mode-abbrev-table
  '(("inclx" "" skeleton-c-mode-incl-func 0)))
(define-abbrev-table 'c-mode-abbrev-table
  '(("inclx" "" skeleton-c-mode-incl-func 0)))

;; Generate include user
(define-skeleton skeleton-c-mode-incu-func
  "generate #include \"\" automatic" nil
  "#include \""
  (completing-read "Include File:"
                   (mapcar '(lambda (f) (list f ))
                           (apply 'append
                                  (mapcar
                                   '(lambda (dir)
                                      (directory-files dir))
                                   (list "./")))))
  
  "\"")
(define-abbrev-table 'c++-mode-abbrev-table
  '(("incux" "" skeleton-c-mode-incu-func 0)))
(define-abbrev-table 'c-mode-abbrev-table
  '(("incux" "" skeleton-c-mode-incu-func 0)))

;; ;; Generate using
;; (define-skeleton skeleton-c-mode-using-func
;;   "generate using namesapce " nil
;;   "using namespace " _)
;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("usings" "" skeleton-c-mode-using-func 0)))

;;=========================================================================
