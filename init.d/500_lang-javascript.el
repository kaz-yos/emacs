;;; 500_lang-javascript.el ---                       -*- lexical-binding: t; -*-
;;; 
;;; javascript and related languages


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
