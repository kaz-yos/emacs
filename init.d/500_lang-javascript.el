;;; 500_lang-javascript.el ---                       -*- lexical-binding: t; -*-
;;; 
;;; javascript and related languages


;;;
;;; js2-mode.el
;; Improved JavaScript editing mode for GNU Emacs
;; https://github.com/mooz/js2-mode
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
;;
;;; ac-js2.el
(add-hook 'js2-mode-hook 'ac-js2-mode)
;;


;;;
;;; js3-mode.el
;; A chimeric fork of js2-mode and js-mode
;; https://github.com/thomblake/js3-mode
(require 'js3-mode)


;;;
;;; js-comint.el
(require 'js-comint)
;; Set inferior-js-program-command to the execution command for running your javascript REPL
;; Use Node.js https://nodejs.org/en/
(setq inferior-js-program-command "node --interactive")


;;; moz.el
;; Lets current buffer interact with inferior mozilla
;; https://github.com/bard/mozrepl/wiki/Emacs-integration
;; C-c C-s: open a MozRepl interaction buffer and switch to it
;; C-c C-l: save the current buffer and load it in MozRepl
;; C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;; C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;; C-c C-r: send the current region to MozRepl
;; In the interaction buffer:
;; C-c c: insert the current name of the REPL plus the dot operator (usually repl.)
;;
;;; moz-controller.el
;; Control Firefox from Emacs
;; https://github.com/RenWenshan/emacs-moz-controller


;;; 
;;; COFFEESCRIPT-RELATED
;;; coffee-mode.el
;; https://github.com/defunkt/coffee-mode
(use-package coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)
         ("\\.iced\\'" . coffee-mode)
         ("Cakefile\\'" . coffee-mode)
         ("\\.cson\\'" . coffee-mode))
  :interpreter ("coffee" . coffee-mode)
  :config
  (custom-set-variables
   '(coffee-tab-width 2)
   '(coffee-args-compile '("-c" "--bare")))
  (bind-key "M-r" 'coffee-compile-buffer     coffee-mode-map)
  (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map))
