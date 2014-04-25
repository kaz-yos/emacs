;;; -*- lexical-binding: t; -*-
;;; EMACS LISP
;;; elisp programming configurations
;; Non-nil means enter debugger if an error is signaled.
;; (setq debug-on-error t)
;;
;;
;;; redshank to facilitate elisp/clisp
;; http://www.foldr.org/~michaelw/emacs/redshank/
(require 'redshank)
;;
;;
;;; SLIME-like navigation for elisp
;; This package provides Slime's convenient "M-." and "M-," navigation
;; in `emacs-lisp-mode', together with an elisp equivalent of
;; `slime-describe-symbol', bound by default to `C-c C-d d`.
;; Usage:
;; Enable the package in elisp and ielm modes as follows:
;; This is optional if installed via package.el
(require 'elisp-slime-nav)
;; Hook
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
;;
;;
;;; Auto byte-compile .el files at saving
;; http://www.emacswiki.org/emacs/auto-async-byte-compile.el
;; http://d.hatena.ne.jp/rubikitch/20100423/bytecomp
(require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|init.el\\|/init.d/\\|/programming/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;;
;;
;;; auto-complete-emacs-lisp.el 2013-09-08
;; https://github.com/rik0/tentative-configuration-emacs/blob/master/emacs.d/auto-complete-emacs-lisp.el
(require 'auto-complete-emacs-lisp)
;; Turn on and off
(define-key emacs-lisp-mode-map (kbd "C-c a") 'auto-complete-mode)
;;
;;
;;; lispxmp.el to evaluate sexp within .el
;; evaluate within script
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "s-e") 'lispxmp)
(define-key lisp-interaction-mode-map (kbd "C-c e") 'lispxmp)
(define-key lisp-interaction-mode-map (kbd "s-e") 'lispxmp)



;;;
;;; CLOJURE SETTINGS
;; http://mkamotsu.hateblo.jp/entry/2013/10/31/142105
;; http://www.braveclojure.com/using-emacs-with-clojure/
;;
;;; cider.el
;; https://github.com/clojure-emacs/cider
(require 'cider)
;;
;; Configurations
;; https://github.com/clojure-emacs/cider#configuration
;;
;; Enable eldoc in Clojure buffers:
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
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
;;; ac-nrepl
;; https://github.com/clojure-emacs/ac-nrepl
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook
	  '(lambda ()
	     (ac-nrepl-setup)
	     ;; Add ac-source-filename for directory name completion
	     (add-to-list 'ac-sources 'ac-source-filename)))
;;
(add-hook 'cider-mode-hook
	  '(lambda ()
	     (ac-nrepl-setup)
	     ;; Add ac-source-filename for directory name completion
	     (add-to-list 'ac-sources 'ac-source-filename)))
;;
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
;;
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
;;; clojure-test-mode.el
(require 'clojure-test-mode)
;;
;;
;;; 4clojure.el
(require '4clojure)
;;
;;
;;; C-c v for help
(defun cider-help-for-symbol ()
  "Provide help for a symbol in the REPL."

  (interactive)
  ;; Define variables
  (let* ((name-symbol (thing-at-point 'symbol t))
	 (doc-string (concat "(doc " name-symbol ")"))
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



;;;
;;; SLIME for non-elisp lisps
;;; slime.el
;; Common Lisp hyperspec via homebrew
;; http://www.lispworks.com/documentation/common-lisp.html
(eval-after-load "slime"
  '(progn
     ;; (setq common-lisp-hyperspec-root
     ;;       "/usr/local/share/doc/hyperspec/HyperSpec/")
     (setq common-lisp-hyperspec-root
	   "http://www.harlequin.com/education/books/HyperSpec/")
     (setq common-lisp-hyperspec-symbol-table
           (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
     (setq common-lisp-hyperspec-issuex-table
           (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))))
;;
;; http://www.common-lisp.net/project/slime/
;; http://dev.ariel-networks.com/wp/archives/462
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
;;
;; Common lisp (installed via homebrew)
;; (setq inferior-lisp-program "/usr/local/bin/clisp")
;;
;; 2.5.2 Multiple Lisps (first one is the default)
;; http://common-lisp.net/project/slime/doc/html/Multiple-Lisps.html
;; (NAME (PROGRAM PROGRAM-ARGS...) &key CODING-SYSTEM INIT INIT-FUNCTION ENV)
;; NAME is a symbol and is used to identify the program.
;; PROGRAM is the filename of the program. Note that the filename can contain spaces.
;; PROGRAM-ARGS is a list of command line arguments.
;; CODING-SYSTEM the coding system for the connection. (see slime-net-coding-system)x
(setq slime-lisp-implementations
      '((clisp  ("/usr/local/bin/clisp"))	; first one is the default
	(sbcl   ("/usr/local/bin/sbcl"))
	(scheme ("/usr/local/bin/scheme"))))
;;
;;; auto-complete for SLIME 2014-02-25
(require 'ac-slime)
(add-hook 'slime-mode-hook      'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
;;
;;


;;;
;;; SCHEME MODE
