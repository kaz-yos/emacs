;;; 500_lang-common.el ---                           -*- lexical-binding: t; -*-
;; Settings useful for programming in general

;;;
;;; electric-spacing.el
;; https://github.com/zk-phi/electric-spacing
(use-package electric-spacing
  :disabled t)


;;;
;;; column-marker.el
;; http://www.emacswiki.org/emacs/column-marker.el
(use-package column-marker
  :commands column-marker80
  :config
  (defun column-marker80 ()
    (interactive)
    (column-marker-2 80)))
