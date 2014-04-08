;;;
;;; Emacs Lisp
;;; eval short cut
;; C-RET for eval-region in elisp mode 2013-12-22
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-region)

;;; SLIME-like navigation for elisp
;; This package provides Slime's convenient "M-." and "M-," navigation
;; in `emacs-lisp-mode', together with an elisp equivalent of
;; `slime-describe-symbol', bound by default to `C-c C-d d`.
;; Usage:
;; Enable the package in elisp and ielm modes as follows:
(require 'elisp-slime-nav) ;; optional if installed via package.el
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;;; Auto byte-compile .el files at saving
;; http://www.emacswiki.org/emacs/auto-async-byte-compile.el
;; http://d.hatena.ne.jp/rubikitch/20100423/bytecomp
(require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|init.el\\|/init.d/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; auto-complete-emacs-lisp.el 2013-09-08
;; https://github.com/rik0/tentative-configuration-emacs/blob/master/emacs.d/auto-complete-emacs-lisp.el
(require 'auto-complete-emacs-lisp)


;;; lispxmp.el to evaluate sexp within .el
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)


;;;
;;; SLIME for non-elisp lisp
;;; slime.el
;; http://www.common-lisp.net/project/slime/
;; http://dev.ariel-networks.com/wp/archives/462
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
;;
;; elisp as inferior-lisp-program	; did not work
;; http://stackoverflow.com/questions/6687721/repl-for-emacs-lisp
;;(setq inferior-lisp-program "/usr/local/bin/emacs --batch --eval '(while t (print (eval (read))))'")
;;
;; Common lisp (installed via homebrew)
(setq inferior-lisp-program "/usr/local/bin/clisp")
;;
;;; auto-complete for SLIME 2014-02-25
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
;;
;;; Define REPL start up function
(defun my-slime-start ()
  (interactive)
  (if (not (member "*slime-repl clisp*" (mapcar #'buffer-name (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))

        (slime)						; Activate slime REPL

        (set-window-buffer w1 "*slime-repl clisp**")	; REPL on the left (w1)
        (set-window-buffer w2 w1name)			; script on the right (w2)
	(select-window w2)				; Select script (w2) Added
	)))
;;
;;; Define a flexible eval function.
(defun my-slime-eval ()
  (interactive)
  (my-slime-start)
  (if (and transient-mark-mode mark-active)			; Check if selection is present
      ;; (apply #'slime-eval-region (sort (list (point) (mark)) #'<))
      (slime-eval-region (point) (mark))			; If selected, send region
    ;; If not selected, do all the following
    (beginning-of-line)						; Move to the beginning of line
    (if (looking-at "(defun ")					; Check if the first word is def (function def)
	(slime-eval-defun)					; Send whole def
      ;; If it is not def, do all the following
      (end-of-line)						; Move to the end of line
      (slime-eval-last-expression)				; Eval the one before
      )
    ))
;;
;;
;; define keys
(add-hook 'slime-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'my-slime-eval)))


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
;;
;;
;;; 4clojure.el
(require '4clojure)
;;
;;
;;; ac-cider-compliment.el
;;     (load "path/to/ac-nrepl-compliment.el")
;;     (require 'ac-cider-compliment)
;;     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;     (add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'cider-mode))
;;
;;; Define REPL start up function
(defun my-cider-start ()
  (interactive)
  (if (not (member "*cider-repl localhost*" (mapcar #'buffer-name (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))

        (cider-jack-in)					; Activate cider REPL

        (set-window-buffer w1 "*cider-repl localhost*")	; REPL on the left (w1)
        (set-window-buffer w2 w1name)			; script on the right (w2)
	(select-window w2)				; Select script (w2) Added
	)))
;;; Define a flexible eval function.
(defun my-cider-eval ()
  (interactive)
  (my-cider-start)
  (if (and transient-mark-mode mark-active)			; Check if selection is present
      (cider-eval-region (point) (mark))			; If selected, send region
    ;; If not selected, do all the following
    (beginning-of-line)						; Move to the beginning of line
    (if (looking-at "(defun ")					; Check if the first word is def (function def)
	(cider-eval-defun-at-point)				; Send whole def
      ;; If it is not def, do all the following
      (end-of-line)						; Move to the end of line
      (cider-eval-last-sexp)					; Eval the one before
      )))
;;
;; define keys
(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'my-cider-eval)))
