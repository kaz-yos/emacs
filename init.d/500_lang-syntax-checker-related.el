;;; 500_lang-syntax-checker-related.el ---           -*- lexical-binding: t; -*-

;;;
;;; FLYMAKE-RELATED
;;;  flymake.el
(use-package flymake
  :commands (flymake-mode)
  :config)


;;;
;;; FLYCHECK-RELATED
;;;  flycheck.el
;; http://www.flycheck.org/en/latest/
;; https://github.com/flycheck/flycheck
;; Supported languages
;;  http://www.flycheck.org/en/latest/languages.html
;; Troubleshooting
;;  http://www.flycheck.org/en/latest/user/troubleshooting.html
;; Developer’s Guide
;;  http://www.flycheck.org/en/latest/developer/developing.html
;;
;; M-x `flycheck-verify-setup' in an appropriate buffer to diagnose issues.
(use-package flycheck
  :config
  (global-flycheck-mode +1))

;;;  flycheck-pos-tip.el
;; https://github.com/flycheck/flycheck-pos-tip
(use-package flycheck-pos-tip
  :disabled t
  :after flycheck
  :config
  (setq flycheck-display-errors-function
        #'flycheck-pos-tip-error-messages)
  (flycheck-pos-tip-mode +1))

;;;  flycheck-package.el
;; Flycheck checker for elisp package metadata
;; https://github.com/purcell/flycheck-package
(use-package flycheck-package
  :after flycheck
  :commands (flycheck-package-setup)
  :config
  (flycheck-package-setup))

;;;  flycheck-cask.el
;; https://github.com/flycheck/flycheck-cask
(use-package flycheck-cask
  :after flycheck
  :commands (flycheck-cask-setup)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))
