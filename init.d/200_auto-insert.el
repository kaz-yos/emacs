;;; 200_auto-insert.el ---                           -*- lexical-binding: t; -*-

;;; Templates by autoinsert.el
;;
;; autoinsert.el
(use-package autoinsert
  :config
  ;; Directory from which auto-inserted files are taken.
  (setq auto-insert-directory (concat user-emacs-directory
                                      "autoinsert/"))
  ;; Definitions by the file extensions
  ;; (define-auto-insert "^ui\\.R$"      "ui.R")
  ;; (define-auto-insert "^server\\.R$"  "server.R")
  (define-auto-insert "\\.R$"         "rscript.R")
  (define-auto-insert "\\.Rmd$"       "knitr.Rmd")
  (define-auto-insert "\\.Rnw$"       "knitr.Rnw")
  (define-auto-insert "\\.sas$"       "sas.sas")
  (define-auto-insert "\\.sh$"        "shell.sh")
  (define-auto-insert "\\.tex$"       "latex.tex")
  (define-auto-insert "\\.gitignore$" ".gitignore")
  (define-auto-insert "project\\.clj" "project.clj")
  (define-auto-insert "\\.rkt"        "racket.rkt")
  (define-auto-insert "\\.hy$"        "hy.hy")
  (define-auto-insert "\\.stan$"      "stan.stan")
  (define-auto-insert "\\.org$"       "org.org")
  ;; Activate
  (auto-insert-mode))
