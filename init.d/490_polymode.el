;;; polymode.el
;; Versatile multiple modes with extensive literate programming support
;; https://github.com/vitoshka/polymode
;;
;;; R-RELATED
(use-package poly-R
  ;; Do not defer as it is required by ess config
  :demand
  ;; These mode association is overwritten by ess-site
  ;; So include the same thing in ess-site :mode
  :mode (("\\.Rnw" . poly-noweb+r-mode)
         ("\\.Rmd" . poly-markdown+r-mode))
  :config
  ;; Auto revert for .Rmd
  (add-hook 'poly-markdown+r-mode-hook 'turn-on-auto-revert-mode)
  ;; Key config
  (bind-key "C-c n" 'polymode-next-chunk-same-type     polymode-mode-map)
  (bind-key "C-c p" 'polymode-previous-chunk-same-type polymode-mode-map))
;;
;;
;;; MARKDOWN-related
(use-package poly-markdown
  :mode ("\\.md" . poly-markdown-mode))
