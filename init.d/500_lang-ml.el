;;; 500_lang-ml.el ---                               -*- lexical-binding: t; -*-
;; Configuration for ML family of languages

;;; 
;;; sml-mode.el
(use-package sml-mode
  :mode "\\.sml"
  :config
  ;; ac setting
  (defun inferior-sml-mode-ac-setting ()
    (auto-complete-mode 1)
    (add-to-list 'ac-sources 'ac-source-filename))
  ;;
  (add-hook 'inferior-sml-mode-hook 'inferior-sml-mode-ac-setting))


;;; 
;;; tuareg.el for OCaml
;; http://forge.ocamlcore.org/projects/tuareg/
(use-package tuareg
  :mode ("\\.ml" . tuareg-mode))
