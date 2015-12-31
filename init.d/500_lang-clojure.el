;;; -*- lexical-binding: t; -*-

;;;
;;; CLOJURE SETTINGS
;; http://mkamotsu.hateblo.jp/entry/2013/10/31/142105
;; http://www.braveclojure.com/using-emacs-with-clojure/
;;
;;; cider.el
;; https://github.com/clojure-emacs/cider
;; Pin to the stable version
;; https://github.com/clojure-emacs/cider#installation-via-packageel
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;;
(require 'cider)
;;
;; Configurations
;; https://github.com/clojure-emacs/cider#configuration
;;
;; Enable eldoc in Clojure buffers
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; Hide special repl buffers
;; (setq nrepl-hide-special-buffers t)
;; To auto-select the error buffer when it's displayed:
(setq cider-auto-select-error-buffer t)
;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
;; (setq cider-repl-pop-to-buffer-on-connect nil)
;; Limit the number of items of each collection
(setq cider-repl-print-length 500)
;;
;; auto-complete-mode toggle
(define-key clojure-mode-map (kbd "C-c a") 'auto-complete-mode)
;;
;;
;;; ac-cider.el
;; https://github.com/clojure-emacs/ac-cider
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'cider-mode))
;; If you want to trigger auto-complete using TAB in CIDER buffers, you may
;; want to use auto-complete in your `completion-at-point-functions':
;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;;
;;; latest-clojure-libraries.el
;; https://github.com/AdamClements/latest-clojure-libraries/
(require 'latest-clojure-libraries)
;;
;;
;;; cider-toggle-trace
(require 'cider-tracing)
;;
;;
;;; clojure-cheatsheet.el
(require 'clojure-cheatsheet)
;;
;;
;;; clojure-test-mode.el 2014-10-27 deprecated
;; (require 'clojure-test-mode)
;;
;;
;;; 4clojure.el
(require '4clojure)
;;
;;
;;; C-c C-v for help and examples
(defun cider-help-for-symbol ()
  "Provide help for a symbol in the REPL."
  (interactive)
  ;; Define variables
  (let* ((name-symbol (thing-at-point 'symbol t))
	 (doc-string (concat "(doc " name-symbol ")"))
	 (eg-string  (concat "(clojuredocs " name-symbol ")"))
	 (script-window (selected-window)))
    ;; move to repl
    (cider-switch-to-repl-buffer)
    ;; Insert the (doc fun-name)
    (insert doc-string)
    ;; Execute
    (cider-repl-return)
    ;; Move back to the script window
    (select-window script-window)))
;;
(define-key clojure-mode-map (kbd "C-c C-v") 'cider-help-for-symbol)
;;
;;
;;; C-c C-g for type
(defun cider-type-for-symbol ()
  "Provide type for a symbol in the REPL."

  (interactive)
  ;; Define variables
  (let* ((name-symbol (thing-at-point 'symbol t))
	 (type-string (concat "(type " name-symbol ")"))
	 (script-window (selected-window)))

    ;; move to repl
    (cider-switch-to-repl-buffer)

    ;; Insert the (type fun-name)
    (insert type-string)

    ;; Execute
    (cider-repl-return)

    ;; Move back to the script window
    (select-window script-window)))
;;
(define-key clojure-mode-map (kbd "C-c C-g") 'cider-type-for-symbol)
;;
;;
;;; clj-refactor.el
;; (require 'clj-refactor)
;; (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
;;
;; Setup keybindings
;; All functions in clj-refactor have a two-letter mnemonic shortcut. You
;; get to choose how those are bound. Here's how:
;;     (cljr-add-keybindings-with-prefix "C-c C-m")
;;     ;; eg. rename files with `C-c C-m rf`.
;; If you would rather have a modifier key, instead of a prefix, do:
;;     (cljr-add-keybindings-with-modifier "C-s-")
;;     ;; eg. rename files with `C-s-r C-s-f`.
;; If neither of these appeal to your sense of keyboard layout aesthetics, feel free
;; to pick and choose your own keybindings with a smattering of:
;;     (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
;;
;;
;;; clojure-quick-repls.el
;; https://github.com/symfrog/clojure-quick-repls
(require 'clojure-quick-repls)
;;
;;
;;; cider-profile.el
;; nrepl support for thunknyc/profile
;; https://github.com/thunknyc/nrepl-profile;
(require 'cider-profile)
;;
;;
;;; cider-spy.el (this breaks REPL connection?)
;; Lets developers share information on CIDER nREPL sessions
;; https://github.com/jonpither/cider-spy
;; (require 'cider-spy)

