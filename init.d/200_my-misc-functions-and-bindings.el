;; Take current line to top
(defun my-recenter-top ()
  (interactive)
  (recenter "Top")
)
(global-set-key (kbd "C-S-l") 'my-recenter-top)
;;
;; http://ergoemacs.org/emacs/elisp_datetime.html
(defun my-insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert (format-time-string "%Y-%m-%d"))
  )
(global-set-key (kbd "C-c d") 'my-insert-date)
;;
;; replace
(global-set-key (kbd "C-c r") 'replace-string)


;; Surround region
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




