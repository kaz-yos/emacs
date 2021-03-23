;;; -*- lexical-binding: t; -*-

;; Non-nil means enter debugger if an error is signaled.
;; (setq debug-on-error t)


;;;
;;; Default programming utilities
;;;  cl-lib.el
;; GNU Emacs Common Lisp Emulation
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html
;; A part of emacs. Auto-loaded. Here for documentation.
(use-package cl-lib
  :defer t
  ;;
  ;; `setf' extensions
  ;; `gv-define-simple-setter', `gv-define-setter', and `gv-define-expander' are used.
  ;; e.g., `setf' for `buffer-name'
  ;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html#Generalized-Variables
  ;;
  ;; cl- sequence extensions
  ;; e.g., `cl-remove-if-not', `cl-subsetp', `cl-position'
  ;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html#Sequences
  ;;
  ;; cl- conditional extensions
  ;; e.g., `cl-case'
  ;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html#Conditionals
  ;;
  ;; cl- iteration extensions
  ;; e.g., `cl-loop'
  ;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html#Iteration
  ;;
  ;; Creating symbols
  ;; e.g., `cl-gensym'
  ;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html#Creating-Symbols
  )

;;;  gv.el
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generalized-Variables.html
(use-package gv
  :defer t
  ;; The `setf' macro is the most basic way to operate on generalized variables.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Generalized-Variables.html
  ;; Defining new setf forms
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Adding-Generalized-Variables.html
  )


;;;
;;; Contributing a new recipe to MELPA
;; https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org


;;;
;;; CASK-RELATED
;; Project management tool for Emacs
;; https://github.com/cask/cask
;; https://cask.readthedocs.io/en/latest/
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;;;  cask.el
;; Project management tool for Emacs http://cask.readthedocs.io
;; https://github.com/cask/cask/blob/master/cask.el
;;
;; This package is not meant for direct use.
;; Rather it is used by the command-line cask tool.
;; But cask installed via Homebrew uses its own cask.el,
;; thus, this MELPA-installed cask.el is not used.
;; It is installed for reference.
(use-package cask
  :disabled t)

;;;  package-build.el
;; Tools for assembling a package archive https://github.com/melpa/melpa
;; https://github.com/melpa/package-build
(use-package package-build
  :disabled t)


;;;
;;; Programming utilities
;;;  stream.el
;; https://nicolas.petton.fr/blog/stream.html
(use-package stream
  :ensure t
  :commands (stream-cons
             stream-range))


;;;
;;; Code navigation and evaluation
;;;  elisp-slime-nav.el
;; SLIME-like navigation for elisp
;; https://github.com/purcell/elisp-slime-nav
;; This package provides Slime's convenient "M-." and "M-," navigation
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :commands (turn-on-elisp-slime-nav-mode)
  ;; Hook
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;;;  eros.el
;; https://github.com/xiongtx/eros/
(use-package eros
  :ensure t
  :config
  ;; edebug version
  (defun my-eros-edebug-eval-last-sexp (&optional _no-truncate)
    "Replacement for `edebug-eval-last-sexp' that overlays results."
    (interactive "P")
    (eros--eval-overlay
     (let ((edebug-print-length nil)
           (edebug-print-level nil)
           (expr (edebug-last-sexp)))
       ;; Recontructed from the body of `edebug-eval-expression'
       ;; to obtain the result for eros to use.
       (edebug-outside-excursion
        (let ((result (edebug-eval expr)))
          (values--store-value result)
          (princ
           (concat (edebug-safe-prin1-to-string result)
                   (eval-expression-print-format result)))
          ;; Return the value for `eros--eval-overlay'.
          result)))
     (point)))
  (advice-add #'edebug-eval-last-sexp
              :override #'my-eros-edebug-eval-last-sexp)
  ;;
  ;; Activate
  (eros-mode 1))

;;;  macrostep.el
;; https://github.com/joddie/macrostep
(use-package macrostep
  :ensure t
  :commands (macrostep-mode
             macrostep-expand))

;;;  nameless.el
;; https://github.com/Malabarba/Nameless
;; http://endlessparentheses.com/nameless-less-is-more.html
(use-package nameless
  :ensure t
  :commands (nameless-mode))


;;;
;;; Debugging

;;;  debug.el
;; Part of emacs
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Debugger.html
(use-package debug
  :commands (debug-on-entry
             cancel-debug-on-entry
             debug-on-variable-change
             cancel-debug-on-variable-change))

;;;  edebug.el
;; Part of emacs
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html
(use-package edebug
  :commands (edebug-eval-top-level-form
             edebug-on-entry
             cancel-edebug-on-entry))

;;;  edebug-inline-result.el
;; https://github.com/stardiviner/edebug-inline-result
(use-package edebug-inline-result
  :ensure t
  :hook (edebug-mode . edebug-inline-result-mode)
  :config
  (use-package popup)
  (setq edebug-inline-result-backend 'popup))


;;;
;;; Good practice related

;;;  buttercup.el
;; Behavior-Driven Emacs Lisp Testing
;; https://github.com/jorgenschaefer/emacs-buttercup
(use-package buttercup
  :ensure t
  :hook (emacs-lisp-mode . buttercup-minor-mode)
  :commands (buttercup-minor-mode))

;;;  flycheck-buttercup.el
;; Additional matchers. Part of flycheck.
;; https://github.com/flycheck/flycheck/blob/master/flycheck-buttercup.el
(use-package flycheck-buttercup
  :ensure flycheck
  :after buttercup)

;;;  assess.el
;; https://github.com/phillord/assess
(use-package assess
  :ensure t
  :commands (assess-with-filesystem)
  :config)

;;;  package-lint.el
;; A linting library for elisp package metadata
;; https://github.com/purcell/package-lint
(use-package package-lint
  :ensure t
  :commands (package-lint-current-buffer))

;;;  checkdoc.el
(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode))

;;;  elisp-lint.el
;; basic linting for Emacs Lisp
;; https://github.com/gonewest818/elisp-lint
(use-package elisp-lint
  :ensure t
  :commands (elisp-lint-file)
  :config
  ;; Alist of symbols and their indent specifiers.
  (setq elisp-lint-indent-specs nil))


;;;
;;; Demonstration related

;;;  demo-it.el
(use-package demo-it
  :ensure t
  :config)

;;;  package-demo.el
;; https://github.com/vermiculus/package-demo
