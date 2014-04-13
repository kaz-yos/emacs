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
  (let* (dummy)
      ;;(window1 window2 name-script-buffer name-repl-buffer)
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
	  ;; window1: left (still selected), window2: right
	  (setq window2 (split-window window1 nil "right"))

	  ;; Activate the REPL (Interactive functions are used)
	  (call-interactively fun-repl-start)

	  ;; If using cider-jack-in, wait for connection.
	  (if (eq fun-repl-start 'cider-jack-in)
	      (progn (when (not (cider-connected-p))
		       (message "waiting for cider...")
		       (sit-for 4))))
	  
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
