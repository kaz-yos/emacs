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
(global-set-key (kbd "C-c r") 'replace-string)
;;
;; (global-set-key (kbd "C-c m") 'comment-region)	; use M-; (comment-dwim)
