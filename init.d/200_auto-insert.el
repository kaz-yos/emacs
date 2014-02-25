;;; Templates by autoinsert.el
;;
;; autoinsert.el
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/autoinsert/")
;;
;; Definitions by the file extensions
(define-auto-insert "\\.R$"         "Rscript.R")
(define-auto-insert "\\.Rmd$"       "knitr.Rmd")
(define-auto-insert "\\.Rnw$"       "knitr.Rnw")
(define-auto-insert "\\.sas$"       "SAS.sas")
(define-auto-insert "\\.sh$"        "shell.sh")
(define-auto-insert "\\.tex$"       "LaTeX.tex")
(define-auto-insert "\\.gitignore$" ".gitignore")
