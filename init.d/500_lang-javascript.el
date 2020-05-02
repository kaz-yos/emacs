;;; 500_lang-javascript.el ---                       -*- lexical-binding: t; -*-
;;;
;;; javascript and related languages


;; Setting up Emacs for JavaScript (part #1)
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html


;;;
;;; js2-mode.el
;; Improved JavaScript editing mode for GNU Emacs
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :ensure t
  :mode (("\\.json$" . js-mode)
         ("\\.js$" . js-mode))
  ;;
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  ;; highlight
  (setq js2-highlight-level 3)
  ;; ac-js2.el
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  ;;
;;; js3-mode.el
  ;; A chimeric fork of js2-mode and js-mode
  ;; https://github.com/thomblake/js3-mode
  (use-package js3-mode
    :ensure t)
;;; js-comint.el
  (use-package js-comint
    :ensure t)
  ;; Set inferior-js-program-command to the execution command for running your javascript REPL
  ;; Use Node.js https://nodejs.org/en/
  (setq inferior-js-program-command "node --interactive"))
