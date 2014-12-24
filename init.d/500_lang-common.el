;;; 500_lang-common.el ---                           -*- lexical-binding: t; -*-
;; Settings useful for programming in general

;;;
;;; electric-spacing.el
;; https://github.com/zk-phi/electric-spacing
(require 'electric-spacing)


;;;
;;; column-marker.el
;; http://www.emacswiki.org/emacs/column-marker.el
(require 'column-marker)
;; Define a function for column 80 highlighting
(defun column-marker80 ()
  (interactive)
  (column-marker-2 80))
