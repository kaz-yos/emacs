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


;;; sqlplus.el		; User friendly interface to SQL*Plus and support for PL/SQL compilation
;;  The following commands should be added to a global initialization
;;  file or to any user's .emacs file to conveniently use
;;  sqlplus-mode:
;;
(require 'sqlplus)
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))
;;
;;  If you want PL/SQL support also, try something like this:
;;
;;  (require 'plsql)
;;  (setq auto-mode-alist
;;    (append '(("\\.pls\\'" . plsql-mode) ("\\.pkg\\'" . plsql-mode)
;; 		("\\.pks\\'" . plsql-mode) ("\\.pkb\\'" . plsql-mode)
;; 		("\\.sql\\'" . plsql-mode) ("\\.PLS\\'" . plsql-mode) 
;; 		("\\.PKG\\'" . plsql-mode) ("\\.PKS\\'" . plsql-mode)
;; 		("\\.PKB\\'" . plsql-mode) ("\\.SQL\\'" . plsql-mode)
;; 		("\\.prc\\'" . plsql-mode) ("\\.fnc\\'" . plsql-mode)
;; 		("\\.trg\\'" . plsql-mode) ("\\.vw\\'" . plsql-mode)
;; 		("\\.PRC\\'" . plsql-mode) ("\\.FNC\\'" . plsql-mode)
;; 		("\\.TRG\\'" . plsql-mode) ("\\.VW\\'" . plsql-mode))
;; 	      auto-mode-alist ))
;;
;;  M-x sqlplus will start new SQL*Plus session.
;;
;;  C-RET   execute command under point
;;  S-C-RET execute command under point and show result table in HTML 
;;          buffer
;;  M-RET   explain execution plan for command under point
;;  M-. or C-mouse-1: find database object definition (table, view
;;          index, synonym, trigger, procedure, function, package)
;;          in filesystem
;;  C-cC-s  show database object definition (retrieved from database)
