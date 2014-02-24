;;; SQL support				; external dependency
;; http://www.emacswiki.org/emacs/SqlMode
;; http://www.acsu.buffalo.edu/~taowei/wiki/emacs_sql.html
(setq sql-product "mysql")	; for coloring
(setq sql-mysql-program "/usr/local/bin/mysql")
;;
;; sql-indent via package.el
(eval-after-load "sql"
  '(load-library "sql-indent"))
;;
;;
;; Define the MYSQL start up function
(defun my-sql-start-mysql ()
  (interactive)
  (if (not (member "*SQL*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (sql-mysql)			; Start MySQL
        (set-window-buffer w1 "*SQL*")	; SQL on the left (w1)
        (set-window-buffer w2 w1name)	; script on the right (w2)
	(select-window w2)		; Select script (w2) Added
	)))
;;
;; Define the mysql eval function
(defun my-sql-eval ()
  (interactive)
  (my-sql-start-mysql)
  (if (and transient-mark-mode mark-active)	; Check if selection is available
      (sql-send-region (point) (mark))
    (progn
      ;; (beginning-of-line)
      (sql-send-region (point-at-bol) (point-at-eol))
      (next-line))
    ))
;;
;; SQL mode hook
(add-hook 'sql-mode-hook
          '(lambda()
	     (local-set-key (kbd "<S-return>") 'my-sql-eval)
	     (local-set-key (kbd "<C-return>") 'my-sql-eval)
	     ))
;; iSQL mode hook
(add-hook 'sql-interactive-mode-hook
          '(lambda()
	     ;; (local-set-key (kbd "<S-return>") 'my-sql-eval)
	     ;; (local-set-key (kbd "<C-return>") 'my-sql-eval)
	     ))
