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
;;; Code navigation and evaluation
;;;  elisp-slime-nav.el
;; SLIME-like navigation for elisp
;; https://github.com/purcell/elisp-slime-nav
;; This package provides Slime's convenient "M-." and "M-," navigation
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands (turn-on-elisp-slime-nav-mode)
  ;; Hook
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;;;  eros.el
;; https://github.com/xiongtx/eros/tree/dd8910279226259e100dab798b073a52f9b4233a
(use-package eros
  :config
  (eros-mode 1))

;;;  lispxmp.el
;; https://www.emacswiki.org/emacs/lispxmp.el
;; M-; M-; to insert ; =>
;; M-; is for paredit-comment-dwim, which is advised by lispxmp.el.
(use-package lispxmp
  :commands (lispxmp))

;;;  macrostep.el
;; https://github.com/joddie/macrostep
(use-package macrostep
  :commands (macrostep-mode
             macrostep-expand))

;;;  nameless.el
;; https://github.com/Malabarba/Nameless
;; http://endlessparentheses.com/nameless-less-is-more.html
(use-package nameless
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


;;;
;;; Good practice related
;;;

;;;  buttercup.el
;; Behavior-Driven Emacs Lisp Testing
;; https://github.com/jorgenschaefer/emacs-buttercup
(use-package buttercup
  :hook (emacs-lisp-mode . buttercup-minor-mode)
  :commands (buttercup-minor-mode))

;;;  flycheck-buttercup.el
;; Additional matchers. Part of flycheck.
;; https://github.com/flycheck/flycheck/blob/master/flycheck-buttercup.el
(use-package flycheck-buttercup
  :after buttercup)

;;;  package-lint.el
;; A linting library for elisp package metadata
;; https://github.com/purcell/package-lint
(use-package package-lint
  :commands (package-lint-current-buffer))

;;;  checkdoc.el
(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode))

;;;  elisp-lint.el
;; basic linting for Emacs Lisp
;; https://github.com/gonewest818/elisp-lint
(use-package elisp-lint
  :commands (elisp-lint-file)
  :config
  ;; Alist of symbols and their indent specifiers.
  (setq elisp-lint-indent-specs nil))
