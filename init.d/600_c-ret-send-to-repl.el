;;; 600_c-ret-send-to-repl.el --- Send code to REPL  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



;;;
;;; COMMON ELEMENTS
;;; my-matching-elements
(defun my-matching-elements (regexp list)
  "Return a list of elements matching the REGEXP in the LIST."
  ;; emacs version of filter
  (delete-if-not
   ;; predicate: non-nil if an element matches the REGEXP
   (lambda (elt) (string-match regexp elt))
   list))
;;
;;
;;; my-start-repl
;; A function to start a REPL if not already available
;; https://stat.ethz.ch/pipermail/ess-help/2012-December/008426.html
;; http://t7331.codeinpro.us/q/51502552e8432c0426273040
(defun my-repl-start (repl-buffer-regexp fun-repl-start)
  "Start an REPL using a function specified in FUN-REPL-START,
if a buffer named REPL-BUFFER-REGEXP is not already available."
  (interactive)
  ;; Create local variables
  (let* (window1 window2 name-script-buffer name-repl-buffer)
    (if (not (my-matching-elements repl-buffer-regexp (mapcar #'buffer-name (buffer-list))))
	(progn
	  ;; C-x 1 Keep only the window from which this function was called.
	  (delete-other-windows)

	  ;; Make window1 keep the selected (only) window
	  (setq window1 (selected-window))
	  ;; Make name-script-buffer keep the selected (only) buffer
	  (setq name-script-buffer (buffer-name))
	  ;; (split-window &optional WINDOW SIZE SIDE)
	  ;; Split window1 (only one) without size, and create a new window on the right.
	  ;; Use the return value (new window) for window2.
	  ;; window1: left (still selected), window2: right
	  (setq window2 (split-window window1 nil "right"))

	  ;; Activate the REPL (Interactive functions are used)
	  (call-interactively fun-repl-start)

	  ;; If using cider-jack-in, wait for connection.
	  (if (eq fun-repl-start 'cider-jack-in)
	      (progn (when (not (cider-connected-p))
		       (message "waiting for cider...")
		       (sit-for 5))))
	  
	  ;; Make name-repl-buffer keep the selected buffer (REPL)
	  ;; This does not work for python/clojure
	  (setq name-repl-buffer (buffer-name))

	  ;; ;; REPL on the left (window1)  ; Not really necessary.
	  ;; (set-window-buffer window1 name-repl-buffer)
	  ;; Script on the right (window2)
	  (set-window-buffer window2 name-script-buffer)
	  
	  ;; Select the script window on the right (window2)
	  (select-window window2)
	  ))))
;;
;; eg. R interpreter
;; (my-repl-start "*R*" #'R)


;;;
;;; COMMON ELEMENTS FOR LISP LANGUAGES
;;; my-eval-in-repl for lisp languages (used as a skeleton for my-*-eval)
(defun my-eval-in-repl (repl-buffer-regexp fun-repl-start fun-repl-send defun-string)
    "Evaluates expression using a REPL specified by repl-buffer-regexp. Sends
expression using a function specified in fun-repl-start. A function definition
 is detected by a string specified in defun-string and handled accordingly."
  (interactive)
  (let* (;; Save current point
	 (initial-point (point)))

    ;; defined in 200_my-misc-functions-and-bindings.el
    (my-repl-start repl-buffer-regexp fun-repl-start)

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

;;;
;;; EMACS LISP RELATED
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
;;; my-eval-in-ielm
(defun my-eval-in-ielm ()
  "This is a customized version of my-eval-in-repl for ielm."

  (interactive)
  (my-eval-in-repl	; defined in 200_my-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*ielm\\*"
   ;; fun-repl-start
   #'ielm
   ;; fun-repl-send
   #'my-send-to-ielm
   ;; defun-string
   "(defun "))
;;
;;; define keys
;; .el files
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'my-eval-in-ielm)
;; *scratch*
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'my-eval-in-ielm)
;; M-x info
(define-key Info-mode-map (kbd "<C-return>") 'my-eval-in-ielm)



;;;
;;; CIDER FOR CLOJURE RELATED
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
;;; my-eval-in-cider
(defun my-eval-in-cider ()
  "This is a customized version of my-eval-in-repl for cider."

  (interactive)
  (my-eval-in-repl	; defined in 200_my-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*cider-repl.*$"
   ;; fun-repl-start
   'cider-jack-in
   ;; fun-repl-send
   'my-send-to-cider
   ;; defun-string
   "(defn "))
;;
;;; define keys
(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'my-eval-in-cider)))



;;;
;;; SLIME RELATED
;;; my-send-to-slime
;; send to slime
(defun my-send-to-slime (start end)
  "Sends expression to *slime-repl* and have it evaluated."

  (interactive "r")
  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change to slime REPL
    (slime-switch-to-output-buffer)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (slime-repl-return)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))
;;
;;; my-eval-in-slime
(defun my-eval-in-slime ()
  "This is a customized version of my-eval-in-repl for slime."

  (interactive)
  (my-eval-in-repl	; defined in 200_my-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*slime-repl.*$"
   ;; fun-repl-start
   'slime
   ;; fun-repl-send
   'my-send-to-slime
   ;; defun-string
   "(defn "))
;;
;;; define keys
(add-hook 'slime-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'my-eval-in-slime)))


;;;
;;; SCHEME RELATED
;;; my-send-to-scheme
;; send to scheme
(defun my-send-to-scheme (start end)
  "Sends expression to *scheme* and have it evaluated."

  (interactive "r")
  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Move to the other window
    (other-window 1)
    ;; Change to scheme REPL
    (switch-to-scheme t)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (comint-send-input)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))
;;
;;; my-eval-in-scheme
(defun my-eval-in-scheme ()
  "This is a customized version of my-eval-in-repl for scheme."

  (interactive)
  (my-eval-in-repl	; defined in 200_my-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*scheme\\*"
   ;; fun-repl-start
   'run-scheme
   ;; fun-repl-send
   'my-send-to-scheme
   ;; defun-string
   "(define "))
;;
;;; define keys
(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'my-eval-in-scheme)))



;;;
;;; PYTHON-MODE RELATED
;;; my-send-to-python
(defun my-send-to-python (start end)
  "Sends expression to *Python* and have it evaluated."

  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change to Python shell
    (python-shell-switch-to-shell)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (comint-send-input)
    ;; One more time if not ending with \n
    (if (not (equal (substring region-string -1) "\n"))
	(comint-send-input))
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))
;;
;;; my-eval-in-python
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_pythonel/
(defun my-eval-in-python ()
  "Evaluates Python expressions"
  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;; defined in 200_my-misc-functions-and-bindings.el
    (my-repl-start "*Python*" #'run-python)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(my-send-to-python (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of statment
      (python-nav-end-of-statement)
      ;; Go to the end of block
      (python-nav-end-of-block)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  (my-send-to-python (point) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next statement
      (python-nav-forward-statement)				
      
      ;; Activate shell window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the shell
      (python-shell-switch-to-shell)
      ;; Switch back to the script window
      (select-window w-script)					
      )))
;;; define keys
(add-hook 'python-mode-hook		; For Python script
          '(lambda()
	     (local-set-key (kbd "<C-return>") 'my-eval-in-python)
	     ))




;;;


;; (provide '600_c-ret-send-to-repl)
;;; 600_c-ret-send-to-repl.el ends here

