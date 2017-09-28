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
              ("A-s" . polymode-export)
              ("M-s M-s" . polymode-export))
  ;;
  :config
  ;; Auto revert for .Rmd
  (add-hook 'poly-markdown+r-mode-hook 'turn-on-auto-revert-mode)
  ;;
  ;; Execute all R chunks at once from an Rmd document
  ;; https://stackoverflow.com/questions/40894202/execute-all-r-chunks-at-once-from-an-rmd-document
  (defun rmd-send-chunk ()
    "Send current R chunk to ess process."
    (interactive)
    (and (eq (oref pm/chunkmode :mode) 'r-mode)
         (pm-with-narrowed-to-span nil
           (goto-char (point-min))
           (forward-line)
           (ess-eval-region (point) (point-max) nil nil 'R))))
  ;;
  (defun rmd-send-chunks-above ()
    "Send all R code chunks above point."
    (interactive)
    (save-restriction
      (widen)
      (save-excursion
        (pm-map-over-spans
         'rmd-send-chunk (point-min) (point))))))


;;;  MARKDOWN-RELATED
(use-package poly-markdown
  :mode ("\\.md" . poly-markdown-mode))
