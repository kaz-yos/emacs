;;; 500_lang-common.el ---                           -*- lexical-binding: t; -*-
;; Settings useful for programming in general


;;;
;;; aggressive-indent.el
;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :commands (aggressive-indent-mode))


;;;
;;; helm-gtags.el
;; https://github.com/syohex/emacs-helm-gtags
;; http://tuhdo.github.io/c-ide.html
;; $ brew install global # GNU GLOBAL
(use-package helm-gtags
  :commands (helm-gtags-mode)
  ;;
  :init
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-use-input-at-cursor t)
  (setq helm-gtags-pulse-at-cursor t)
  (setq helm-gtags-prefix-key "\C-cg")
  (setq helm-gtags-suggested-key-mapping t)
  ;;
  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  ;;
  :config
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))


;;;
;;; dumb-jump.el
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :config
  ;; https://github.com/jacktasia/dumb-jump#emacs-options
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-use-visible-window t))


;;;
;;; smart-jump.el
;; https://github.com/jojojames/smart-jump
(use-package smart-jump
  ;; Need to demand to activate configuration.
  :demand t
  :commands (smart-jump-go
             smart-jump-back)
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back))
  :config
  ;; Bind M-.  and M-, upon registering `smart-jump'.
  (setq smart-jump-bind-keys t)
  ;; smart-jump-default-mode-list defines modes for the following.
  (smart-jump-setup-default-registers))


;;;
;;; Save and compile function
;; http://stackoverflow.com/questions/2062492/save-and-compile-automatically
(defun save-all-and-compile ()
  "Save buffers with changes before compiling"
  (interactive)
  (save-some-buffers 1)
  (compile compile-command))


;;;
;;; Regular expression handling
;;
;;;  re-builder.el
(use-package re-builder
  :commands (re-builder
             reb-change-syntax
             reb-quit
             rx-reb-mode-backend)
  :hook ((reb-mode . rx-reb-mode-backend-setup))
  :config
  (setq reb-re-syntax 'rx)
  ;;
  (defun rx-reb-mode-backend (command &optional arg &rest ignored)
    ;; The signature (command &optional arg &rest ignored) is mandated.
    "A company backend `rx-constituents' in `reb-mode'.

COMMAND is either one of symbol `interactive',
symbol `prefix', symbol `candidates', and symbol
`annotation'.
ARG is the prefix string to be completed when called
with symbol `candidates'.  ARG is the string to extract
the property from when called with symbol `annotation'.

IGNORED is a placeholder to be ignored.

This backend only comes up with predefined keywords
in the `rx-constituents'.
Group with other backends as necessary.
See the help for `company-backends'."
    ;;
    ;; Making it interactive allows interactive testing.
    (interactive (list 'interactive))
    ;; (cl-case EXPR (KEYLIST BODY...)...)
    ;; Eval EXPR and choose among clauses on that value.
    ;; Here we decide what to do based on COMMAND.
    ;; One of {interactive, prefix, candidates, annotation}
    (cl-case command
      ;; 1. interactive call
      ;; (company-begin-backend BACKEND &optional CALLBACK)
      ;; Start a completion at point using BACKEND.
      (interactive (company-begin-backend 'company-stan-backend))
      ;; 2. prefix command
      ;;  It should return the text that is to be completed.
      ;;  If it returns nil, this backend is not used.
      ;;  Here we need to verify the major mode.
      (prefix (and (eq major-mode 'reb-mode)
                   ;; Ensure not inside a comment.
                   ;; Parse-Partial-Sexp State at POS, defaulting to point.
                   ;; https://emacs.stackexchange.com/questions/14269/how-to-detect-if-the-point-is-within-a-comment-area
                   (not (nth 4 (syntax-ppss)))
                   ;; If point is at the end of a symbol, return it for completion.
                   ;; Otherwise, if point is not inside a symbol, return an empty string.
                   ;; This will give the prefix to be completed.
                   (company-grab-symbol)))
      ;; 3. candidates command
      ;;  This is where we actually generate a list of possible completions.
      ;;  When this is called arg holds the prefix string to be completed
      (candidates
       (cl-remove-if-not
        ;; Retain if matching
        (lambda (c) (string-prefix-p arg c))
        ;; from a long list of all stan object names.
        (mapcar (lambda (elt) (symbol-name (car elt))) rx-constituents)))))
  ;;
  (defun rx-reb-mode-backend-setup ()
    "Add `rx-reb-mode-backend' to `company-backends' buffer-locally."
    (add-to-list (make-local-variable 'company-backends)
                 'rx-reb-mode-backend)))
;;
;;;  rx.el
;; sexp notation for regular expressions
;; https://www.emacswiki.org/emacs/rx
;; https://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/
;;
;; Note M-x re-builder supports `rx'.
;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package rx
  ;; There is no interactive commands.
  :commands (rx))
;;
;;;  xr.el
;; Inverse of rx: convert Emacs string regexps to rx form
;; https://github.com/mattiase/xr
(use-package xr
  :commands (xr))
