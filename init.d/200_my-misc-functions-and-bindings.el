;;; My miscellaneous functions

;;;
;;; Take current line to top
(defun my-recenter-top ()
  (interactive)
  (recenter "Top")
)
(global-set-key (kbd "C-S-l") 'my-recenter-top)
;;

;;;
;;; my-insert-date
;; http://ergoemacs.org/emacs/elisp_datetime.html
(defun my-insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert (format-time-string "%Y-%m-%d"))
  )
(global-set-key (kbd "C-c d") 'my-insert-date)


;;;
;;; replace (kbd "C-c r")
(global-set-key (kbd "C-c r") 'replace-string)


;;;
;;; my-start-repl
;; A function to start a REPL if not already available
;; https://stat.ethz.ch/pipermail/ess-help/2012-December/008426.html
;; http://t7331.codeinpro.us/q/51502552e8432c0426273040
(defun my-repl-start (repl-buffer-name fun-repl-start)
  "Start an REPL using a function specified in fun-repl-start,
if a buffer named repl-buffer-name is not available."
  (interactive)
  ;; Create local variables
  (let* (window1 window2 name-script-buffer name-repl-buffer)
    (if (not (member repl-buffer-name (mapcar #'buffer-name (buffer-list))))
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
	  ;; window1: left, window2: right
	  (setq window2 (split-window window1 nil "right"))

	  ;; Activate the REPL
	  (funcall fun-repl-start)
	  ;; Make name-repl-buffer keep the selected buffer (REPL)
	  (setq name-repl-buffer (buffer-name))

	  ;; REPL on the left (window1)	; This gives an error.
	  (set-window-buffer window1 name-repl-buffer)
	  ;; Script on the right (window2)
	  (set-window-buffer window2 name-script-buffer)
	  ;; Select the script window on the right (window2)
	  (select-window window2)
	  ))))
;;
;; eg. R interpreter
;; (my-repl-start "*R*" #'R)
;;
;;; my-repl-eval for lisp languages
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

;;;
;;; Surround region
;; http://www.emacswiki.org/emacs/SurroundRegion
(defun surround (begin end open close)
  "Put OPEN at START and CLOSE at END of the region.
If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (when (string= close "")
    (setq close open))
  (save-excursion
    (goto-char end)
    (insert close)
    (goto-char begin)
    (insert open)))


;;;
;;; flush-empty-lines
;; (defun flush-empty-lines ()
;;   (interactive)
;;   (flush-lines "^$"))
;;
;; Somehow does not work if not selected from bottom.
(setq flush-empty-lines #'(flush-lines "^$"))
