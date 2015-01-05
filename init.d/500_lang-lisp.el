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
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|init.el\\|/init.d/\\|/programming/")
(setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|/programming/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;;
;;
;;; lispxmp.el to evaluate sexp within .el
;; evaluate within script
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "s-e") 'lispxmp)
(define-key lisp-interaction-mode-map (kbd "C-c e") 'lispxmp)
(define-key lisp-interaction-mode-map (kbd "s-e") 'lispxmp)
;;
;;
;;; anaphora
;;  Summary: anaphoric macros providing implicit temp variables
;; Homepage: http://github.com/rolandwalker/anaphora
;; (require 'anaphora)
;;
;;
;;; Auto-complete in IELM
;; http://www.masteringemacs.org/articles/2010/11/29/evaluating-elisp-emacs/
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)
;;



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
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
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
;;
;; first one is the default
(setq slime-lisp-implementations
      '((sbcl   ("/usr/local/bin/sbcl"))
        (clisp  ("/usr/local/bin/clisp"))))
;;
;;; auto-complete for SLIME 2014-02-25
(require 'ac-slime)
(add-hook 'slime-mode-hook      'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
;;


;;;
;;; SCHEME MODE
;; Also used by geiser.el
;; (require 'scheme)
;; ;; This defines REPL-related functions, including switch-to-scheme
;; (require 'cmuscheme)
;; ;;
;; ;; Use Gauche. REPL name is still *scheme*
;; ;; (setq scheme-program-name "gosh -i")
(add-hook 'scheme-mode-hook '(lambda ()
                               (company-mode -1)))


;;;
;;; RACKET RELATED
;; Racket documentation for emacs
;; http://docs.racket-lang.org/guide/Emacs.html
;;
;; Package Management in Racket
;; http://docs.racket-lang.org/pkg/
;;
;; Install these to make racket-mode work
;; $ raco pkg install rackunit

;;; racket-mode.el
;; https://github.com/greghendershott/racket-mode
;; major mode for Racket. incompatible with geiser minor mode.
;;
;; (require 'racket-mode)

;;; geiser.el
;; Geiser for Racket and Guile Scheme
;; Works as an add-on to the built-in scheme mode
;; http://www.nongnu.org/geiser/
;;
;; Do $ raco pkg install compatibility-lib if installing plt-racket via Homebrew
;; https://github.com/jaor/geiser/issues/39
(require 'geiser)


;;; ac-geiser.el
(require 'ac-geiser)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
;;
(defun my-ac-geiser-setup ()
  (ac-geiser-setup)
  (company-mode -1)
  (define-key geiser-mode-map (kbd "C-.") 'highlight-symbol-at-point))
;;
(add-hook 'geiser-mode-hook      'my-ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'my-ac-geiser-setup)


;;;
;;; egison-mode.el
;; https://github.com/egison/egison/blob/master/elisp/egison-mode.el
;; This is not provided. The mode appears incomplete.
;; (require 'egison-mode)
