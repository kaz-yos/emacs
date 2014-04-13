;;;
;;; Emacs Lisp
;;; elisp programming configurations
;; Non-nil means enter debugger if an error is signaled.
(setq debug-on-error t)
;;
;;; my-repl-eval for lisp languages (used by both my-elisp-eval/my-cider-eval)
(defun my-repl-eval (repl-buffer-name fun-repl-start fun-repl-send defun-string)
    "Evaluates expression using a REPL specified by repl-buffer-name. Sends
expression using a function specified in fun-repl-start. A function definition
 is detected by a string specified in defun-string and handled accordingly."
  (interactive)
  (let* (;; Save current point
	 (initial-point (point)))
    
    ;; defined in 200_my-misc-functions-and-bindings.el
    (my-repl-start repl-buffer-name fun-repl-start)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send to ielm
	(funcall fun-repl-send (point) (mark))
      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Check if the first word is def (function def)
      (if (looking-at defun-string)
	  ;; Use eval-defun if on defun
	  (progn
	    ;; Set a mark there
	    (set-mark (line-beginning-position))
	    ;; Go to the end
	    (forward-sexp)
	    ;; Send to ielm
	    (funcall fun-repl-send (point) (mark))
	    ;; Go to the next expression
	    (forward-sexp))
	;; If it is not def, do all the following
	;; Go to the previous position
	(goto-char initial-point)
	;; Go back one S-exp. (paredit dependency)
	(paredit-backward)
	;; Loop
	(while (not (equal (current-column) 0))
	  ;; Go back one S-exp. (paredit dependency)
	  (paredit-backward))
	;; Set a mark there
	(set-mark (line-beginning-position))
	;; Go to the end of the S-exp starting there
	(forward-sexp)
	;; Eval the S-exp before
	(funcall fun-repl-send (point) (mark))
	;; Go to the next expression
	(forward-sexp)
	))))
;;
;;; my-send-to-ielm
(defun my-send-to-ielm (start end)
  "Sends expression to *ielm* and have it evaluated."

  (interactive "r")
  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change other window to REPL
    (switch-to-buffer-other-window "*ielm*")
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Set mark from beginning
    (set-mark (line-beginning-position))
    ;; Delete the region
    (delete-region (point) (mark))
    ;; Unset transient mark
    (setq mark-active nil)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (ielm-return)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))
;;
;;; my-elisp-eval
(defun my-elisp-eval ()
  "This is a customized version of my-repl-eval for ielm."
  
  (interactive)
  (my-repl-eval	; defined in 200_my-misc-functions-and-bindings.el
   ;; repl-buffer-name
   "*ielm*"
   ;; fun-repl-start
   #'ielm
   ;; fun-repl-send
   #'my-send-to-ielm
   ;;defun-string
   "(defun "))
;;
;; define keys
;; .el files
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'my-elisp-eval)
;; *scratch*
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'my-elisp-eval)
;; M-x info
(define-key Info-mode-map (kbd "<C-return>") 'my-elisp-eval)
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
(setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|init.el\\|/init.d/")
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
(define-key lisp-interaction-mode-map (kbd "C-c e") 'lispxmp)



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
;;; ac-cider-compliment.el
;;     (load "path/to/ac-nrepl-compliment.el")
;;     (require 'ac-cider-compliment)
;;     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;     (add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'cider-mode))
;;
;;; my-send-to-cider
;; send to cider
(defun my-send-to-cider (start end)
  "Sends expression to *cider-repl* and have it evaluated."

  (interactive "r")
  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change to cider REPL
    (cider-switch-to-repl-buffer)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; ;; Set mark from beginning
    ;; (set-mark (line-beginning-position))
    ;; ;; Delete the region
    ;; (delete-region (point) (mark))
    ;; ;; Unset transient mark
    ;; (setq mark-active nil)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (cider-repl-return)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))
;;
;;; my-cider-eval
(defun my-cider-eval ()
  "This is a customized version of my-repl-eval for cider."
  
  (interactive)
  (my-repl-eval	; defined in 200_my-misc-functions-and-bindings.el
   ;; repl-buffer-name
   "*cider-repl localhost*"
   ;; fun-repl-start
   'cider-jack-in
   ;; fun-repl-send
   'my-send-to-cider
   ;;defun-string
   "(defn "))
;;
;; define keys
(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'my-cider-eval)))
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
;;
;;; Define a flexible eval function.
(defun my-slime-eval ()
  (interactive)

  ;; defined in 200_my-misc-functions-and-bindings.el
  (my-repl-start "*slime-repl clisp*" #'slime)

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
