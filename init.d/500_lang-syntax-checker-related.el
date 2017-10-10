;;; SYNTAX CHECKER RELATED

;;;
;;; fly-check.el
;; http://www.flycheck.org/en/latest/
;; https://github.com/flycheck/flycheck
;; Supported languages
;; http://flycheck.readthedocs.org/en/latest/guide/languages.html#supported-languages
;; 3rd party extensions
;; http://flycheck.readthedocs.org/en/latest/guide/introduction.html#rd-party-extensions
(use-package flycheck
  :commands (flycheck-mode
             global-flycheck-mode)
  :init
  ;; (add-hook 'after-init-hook 'global-flycheck-mode)
  ;;
  :config
  ;;; flycheck-pos-tip.el
  ;; http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b#2-5
  (use-package flycheck-pos-tip
    :config
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
    (flycheck-pos-tip-mode +1)))
