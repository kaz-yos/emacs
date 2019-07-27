;;; -*- lexical-binding: t; -*-
;;;
;;; EMACS LISP
;;;  elisp programming configurations
;; Non-nil means enter debugger if an error is signaled.
;; (setq debug-on-error t)

(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

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


;;;  buttercup.el
;; Behavior-Driven Emacs Lisp Testing
;; https://github.com/jorgenschaefer/emacs-buttercup
(use-package buttercup
  :hook (emacs-lisp-mode . buttercup-minor-mode)
  :commands (buttercup-minor-mode))


;;;  package-lint.el
;; https://github.com/purcell/package-lint
(use-package package-lint
  :commands (package-lint-current-buffer))


;;;  checkdoc.el
(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode))


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
