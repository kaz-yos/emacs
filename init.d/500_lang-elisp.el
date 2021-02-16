;;; -*- lexical-binding: t; -*-

;; Non-nil means enter debugger if an error is signaled.
;; (setq debug-on-error t)

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
;; https://github.com/xiongtx/eros/tree/dd8910279226259e100dab798b073a52f9b4233a
(use-package eros
  :ensure t
  :config
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

;;;  let*-setq macro
;; Crude interactive testing
(defmacro let*-setq (&rest body)
  "Sequentially `setq' expressions in `let*'

Change
   (let* ((a 1)
          (b 2))
    ...)
to
   (let* (let*-setq (a 1)
                    (b 2))
    ...)
and evaluate the `let*-setq' expression.
This will globally bind these variables for crude interactive testing."
  `(progn
     ,@(mapcar
        (lambda (let-pair)
          (cons 'setq let-pair))
        body)))


;;;  edebug-inline-result.el
(use-package edebug-inline-result
  :ensure t
  :hook (edebug-mode . edebug-inline-result-mode)
  :config
  (setq edebug-inline-result-backend 'popup))


;;;
;;; Good practice related
;;;

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
