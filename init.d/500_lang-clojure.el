;;; -*- lexical-binding: t; -*-

;;;
;;; CLOJURE SETTINGS
;; http://mkamotsu.hateblo.jp/entry/2013/10/31/142105
;; http://www.braveclojure.com/using-emacs-with-clojure/
;;
;;; clojure-mode
;; http://ccann.github.io/2015/10/18/cider/
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))
;;
;;; cider.el
;; https://github.com/clojure-emacs/cider
;; Pin to the stable version
;; https://github.com/clojure-emacs/cider#installation-via-packageel
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;;
(use-package cider
  :defer t
  :config
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
;;;  latest-clojure-libraries.el
  ;; https://github.com/AdamClements/latest-clojure-libraries/
  ;; Look up the latest version of a library from clojars
  ;; and insert it into your running session.
  (use-package latest-clojure-libraries)
  ;;
;;;  cider-toggle-trace
  (use-package cider-tracing)
  ;;
;;;  clojure-cheatsheet.el
  (use-package clojure-cheatsheet)
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
      ;;
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
;;; C-c C-g for type
  (defun cider-type-for-symbol ()
    "Provide type for a symbol in the REPL."
    (interactive)
    ;; Define variables
    (let* ((name-symbol (thing-at-point 'symbol t))
           (type-string (concat "(type " name-symbol ")"))
           (script-window (selected-window)))
      ;;
      ;; move to repl
      (cider-switch-to-repl-buffer)
      ;; Insert the (type fun-name)
      (insert type-string)
      ;; Execute
      (cider-repl-return)
      ;; Move back to the script window
      (select-window script-window)))
  ;;
  (define-key clojure-mode-map (kbd "C-c C-g") 'cider-type-for-symbol))
