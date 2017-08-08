;;;
;;; polymode.el
;; Versatile multiple modes with extensive literate programming support
;; https://github.com/vitoshka/polymode
;; https://github.com/vspinu/polymode/blob/master/readme.md#basic-usage


;;;  R-RELATED
(use-package poly-R
  ;; Do not defer as it is required by ess config
  :demand
  ;; These mode association is overwritten by ess-site
  ;; So include the same thing in ess-site :mode
  :mode (("\\.Rnw" . poly-noweb+r-mode)
         ("\\.Rmd" . poly-markdown+r-mode))
  :bind (:map polymode-mode-map
              ("C-c n" . polymode-next-chunk-same-type)
              ("C-c p" . polymode-previous-chunk-same-type)
              ("A-n" . polymode-next-chunk-same-type)
              ("A-p" . polymode-previous-chunk-same-type)
              ("A-s" . polymode-export))
  ;;
  :config
  ;; Auto revert for .Rmd
  (add-hook 'poly-markdown+r-mode-hook 'turn-on-auto-revert-mode))


;;;  MARKDOWN-RELATED
(use-package poly-markdown
  :mode ("\\.md" . poly-markdown-mode))
