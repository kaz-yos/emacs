;;; My miscellaneous functions -*- lexical-binding: t; -*-

;;;
;;; Take current line to top
(defun my-recenter-top ()
  "Recenter to the top"
  (interactive)
  (recenter "Top"))
(bind-key "C-S-l" 'my-recenter-top)
(bind-key "l" 'my-recenter-top my-key-map)


;;;
;;; my-insert-date
;; http://ergoemacs.org/emacs/elisp_datetime.html
(defun my-insert-date (u-arg)
  "Insert current date yyyy-mm-dd.

The format is yyyymmdd if a universal argument is given."
  (interactive "P")
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert (if u-arg
              (format-time-string "%Y%m%d")
            (format-time-string "%Y-%m-%d"))))
(bind-key "C-c d" 'my-insert-date)


;;;
;;; my-insert-available-dates
;;
(defun my-convert-to-availability-date (time)
  "Create a date string for Mon-Fri. Blank for Sat/Sun."
  (if (member (format-time-string "%a" time)
              '("Sat" "Sun"))
      ""
    (format-time-string "%b %d (%a)" time)))
;;
(defun my-create-available-dates (weeks)
  "Create date strings for next WEEKS weeks."
  (let* ((cur-time (current-time)))
    ;;
    (thread-last weeks
      ;; convert to days
      (* 7)
      ;; sequence up to days
      (number-sequence 0)
      (seq-map (lambda (inc-day)
                 (days-to-time inc-day)))
      (seq-map (lambda (inc-time)
                 (time-add cur-time inc-time)))
      ;; Keep except Sun
      (seq-filter (lambda (time)
                    (not (member (format-time-string "%a" time)
                                 '("Sun")))))
      (seq-map #'my-convert-to-availability-date))))
;;
(defun my-insert-available-dates (weeks)
  "Insert dates to show available slots."
  (interactive "nHow many weeks?: ")
  (mapc (lambda (str)
          (insert (concat str " \n")))
        (my-create-available-dates weeks)))


;;;
;;; replace (kbd "C-c r")
(bind-key "s-r" 'replace-string)


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
(defun flush-empty-lines (start end)
  "Flush empty lines in the selected region."
  (interactive "r")
  ;; Do not do anything if no selection is present.
  (if (and transient-mark-mode mark-active)
      (flush-lines "^ *$" start end)))
;; key
(bind-key "s-f" 'flush-empty-lines)
(bind-key "f" 'flush-empty-lines 'my-key-map)

;;;
;;; just-one-space
(bind-key "s-o" 'just-one-space)

;;;
;;; revert-buffer
(bind-key "s-v" 'revert-buffer)
(bind-key "v" 'revert-buffer 'my-key-map)



;;;
;;; Suppress messages
;; http://emacs.stackexchange.com/questions/14706/suppress-message-in-minibuffer-when-a-buffer-is-saved
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  ;; If a declare form appears as the first form in the body of a
  ;; defun or defmacro form, SPECS specifies various additional
  ;; information about the function or macro; these go into effect
  ;; during the evaluation of the defun or defmacro form.
  (declare (indent 0))
  ;; If nil, disable message logging.
  `(let ((message-log-max nil))
     ;; The original message is restored to the echo area after BODY has finished.
     (with-temp-message (or (current-message) "")
       ,@body)))



;;;
;;; Define a function to retrieve the selected string or symbol at the point if any.
(defun selection-or-thing-at-point ()
  (cond
   ;; If there is selection use it
   ((and transient-mark-mode
         mark-active
         (not (eq (mark) (point))))
    (let ((mark-saved (mark))
          (point-saved (point)))
      (deactivate-mark)
      (buffer-substring-no-properties mark-saved point-saved)))
   ;; Otherwise, use symbol at point or empty
   (t (format "%s"
              (or (thing-at-point 'symbol)
                  "")))))


;;;
;;; macOS color picker
;; https://gist.github.com/a3ammar/2357d6115ddd999c23e6
;; Only when in macOS GUI
(when (and (eq system-type 'darwin)
           (window-system))
  ;;
  (defun custom-color--choose-action (widget &optional _event)
    "Color-picker widget.
customize `widget-color--chose-action' to not split the screen.
This function is only needed if you want to use color-picker
in Easy Customization"
    (list-colors-display
     nil nil
     `(lambda (color)
        (when (buffer-live-p ,(current-buffer))
	  (widget-value-set ',(widget-get widget :parent) color)
          (pop-to-buffer ,(current-buffer))))))
  ;;
  (defun nscolor2hex (color)
    "Converts colors from `NSColor' format to hex values"
    (concat "#"                           ; Add # at the front
            (mapconcat 'identity          ; concate the list
                       (mapcar '(lambda (x) ;returns ("hex" "hex" "hex")
                                  (let ((col (lsh (string-to-int x) -8)))
                                    (if (< col 16)
                                        (format "0%x" col)
                                      (format "%x" col))))
                               (split-string (s-replace "\"" "" color) ",")) "")))
  ;;
  (defun color-picker (&optional list buffer-name callback)
    "Calls OS X color picker and insert the chosen color. It is really messy because of applyscript"
    (interactive)
    (let ((result
           (do-applescript "tell application \"Finder\"
	activate
set result to \"\"
set x to (choose color)
set result to item 1 of x as string
set result to result & \",\"
set result to result & item 2 of x as string
set result to result & \",\"
set result to result & item 3 of x as string
return result
end tell")))
      (if callback ; For Easy Customization
          (funcall callback (nscolor2hex result))
        (insert (nscolor2hex result)))
      (do-applescript "tell application \"Emacs\" to activate")))
  ;;
  ;; If you want to use `color-picker' in Easy Customization add these
  ;; (defalias 'list-colors-display 'color-picker)
  ;; (defalias 'widget-color--choose-action 'custom-color--choose-action)
  )


;;;
;;; git-log-last-commit-date
(defun git-log-last-commit-date ()
  "Obtain the last commit date for the buffer file."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "git log -n 1 --pretty=format:%cd --date=short -- "
                           buffer-file-name))))


;;;
;;; Put some variable value into the kill ring.
;; http://emacs.1067599.n8.nabble.com/Filename-of-buffer-into-kill-ring-functionality-td62106.html
(defun my-kill-new-string-variable (variable)
  "Make the string value of VARIABLE the latest kill in the kill ring."
  (interactive (let ((current-buffer (current-buffer)))
                 (intern (completing-read "Variable: " obarray
                                          (lambda (symbol)
                                            (with-current-buffer current-buffer
                                              (and (boundp symbol)
                                                   (stringp
                                                    (symbol-value symbol)))))
                                          t))))
  ;; Let symbol-value and kill-new signal errors for unbound variables
  ;; and non-string values, respectively:
  (kill-new (symbol-value variable)))
;;
(defun my-kill-new-buffer-file-name ()
  "Put current `buffer-filename' into the kill ring."
  (interactive)
  (my-kill-new-string-variable 'buffer-file-name))



;;;
;;; Delete ^M
;; https://stackoverflow.com/questions/23712076/how-to-remove-m-in-emacs
(defun my-delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))
