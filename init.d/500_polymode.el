;;; polymode.el
;; Versatile multiple modes with extensive literate programming support
;; https://github.com/vitoshka/polymode
;;
;; R
(require 'poly-R)
;; markdown
(require 'poly-markdown)
;;
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;;
;;; R modes
;; (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;;
;; key config
(define-key polymode-mode-map (kbd "C-c n") 'polymode-next-chunk-same-type)
(define-key polymode-mode-map (kbd "C-c p") 'polymode-previous-chunk-same-type)
