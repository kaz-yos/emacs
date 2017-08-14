;;; My miscellaneous functions -*- lexical-binding: t; -*-

;;;
;;; Take current line to top
(defun my-recenter-top ()
  "Recenter to the top"
  (interactive)
  (recenter "Top"))
(global-set-key (kbd "C-S-l") 'my-recenter-top)


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
(global-set-key (kbd "C-c d") 'my-insert-date)


;;;
;;; replace (kbd "C-c r")
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "s-r") 'replace-string)


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
(global-set-key (kbd "s-f") 'flush-empty-lines)

;;;
;;; just-one-space
(global-set-key (kbd "s-o") 'just-one-space)

;;;
;;; revert-buffer
(global-set-key (kbd "s-v") 'revert-buffer)



;;;
;;; Suppress messages
;; http://qiita.com/itiut@github/items/d917eafd6ab255629346
;; http://emacs.stackexchange.com/questions/14706/suppress-message-in-minibuffer-when-a-buffer-is-saved
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))



;;;
;;; Define a function to retrieve the selected string or symbol at the point if any.
(defun selection-or-thing-at-point ()
  (cond
   ;; If there is selection use it
   ((and transient-mark-mode mark-active
         (not (eq (mark) (point))))
    (buffer-substring-no-properties (mark) (point)))
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
  (defun custom-color--choose-action (widget &optional _event) ; this function is only needed if you want to use color-picker in Easy Customization
    "customize `widget-color--chose-action' to not split the screen"
    (list-colors-display
     nil nil
     `(lambda (color)
        (when (buffer-live-p ,(current-buffer))
	  (widget-value-set ',(widget-get widget :parent) color)
          (pop-to-buffer ,(current-buffer))))))

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

  ;; If you want to use `color-picker' in Easy Customization add these
  (defalias 'list-colors-display 'color-picker)
  (defalias 'widget-color--choose-action 'custom-color--choose-action))
