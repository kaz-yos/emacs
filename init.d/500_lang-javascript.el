;;; 500_lang-javascript.el ---                       -*- lexical-binding: t; -*-
;;; 
;;; javascript and related languages


;;;
;;; js2-mode.el
;;
;; Set up Javascript development environment in Emacs
;; http://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
;;
(require 'js2-mode)
;;
;; files
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;;
;; hooks
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
;;
;; highlight
(setq js2-highlight-level 3)

;;; 
;;; COFFEESCRIPT-RELATED
;;; coffee-mode.el
;; https://github.com/defunkt/coffee-mode
(require 'coffee-mode)
;;
;; coffeescript
(custom-set-variables
 '(coffee-tab-width 2)
 '(coffee-args-compile '("-c" "--bare")))
;;
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))
