;;; SYNTAX CHECKER RELATED

;;; 
;;; fly-check.el
;; https://github.com/flycheck/flycheck
;; Supported languages
;; http://flycheck.readthedocs.org/en/latest/guide/languages.html#supported-languages
;; 3rd party extensions
;; http://flycheck.readthedocs.org/en/latest/guide/introduction.html#rd-party-extensions
(require 'flycheck)
;;
;; Global check
(add-hook 'after-init-hook #'global-flycheck-mode)
;;
;;
;;; flycheck-pos-tip.el
;; http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b#2-5
(require 'flycheck-pos-tip)
