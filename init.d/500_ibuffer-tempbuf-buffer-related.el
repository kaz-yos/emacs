;;; ibuffer use by default
;; http://www.emacswiki.org/emacs/IbufferMode
;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)
;; Switching to ibuffer puts the cursor on the most recent buffer	; Not useful 2014-01-25
;; http://www.emacswiki.org/emacs/IbufferMode#toc13
;; (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
;;   "Open ibuffer with cursor pointed to most recent buffer name"
;;   (let ((recent-buffer-name (buffer-name)))
;;     ad-do-it
;;     (ibuffer-jump-to-buffer recent-buffer-name)))
;; (ad-activate 'ibuffer)
;;
;; Sorting
;; http://mytechrants.wordpress.com/2010/03/25/emacs-tip-of-the-day-start-using-ibuffer-asap/
;; (setq ibuffer-default-sorting-mode 'major-mode)
;; https://github.com/pd/dotfiles/blob/master/emacs.d/pd/core.el
(setq ibuffer-default-sorting-mode 'filename/process	; Sort by filename/process
      ibuffer-show-empty-filter-groups nil)		; Don't show empty groups

;;; ibuffer classification
;; http://www.emacswiki.org/emacs/IbufferMode#toc6
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("DIRED" (mode . dired-mode))
	       ("EMACS" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Packages\\*$")
			 ))
	       ("ESS"   (or
			 (mode . ess-mode)
			 (mode . inferior-ess-mode)
			 (mode . Rd-mode)))
	       ("PYTHON" (or
			  (mode . python-mode)
			  (mode . inferior-python-mode)))
	       ("SHELL"  (or
			  (mode . sh-mode)
			  (mode . shell-mode)
			  (mode . ssh-mode)
			  (mode . eshell-mode)))
	       ("SQL"  (or
			  (mode . sql-mode)
			  (mode . sql-interactive-mode)))
	       ("LISP"	(or
			 (mode . emacs-lisp-mode)))
	       ("TeX"    (or
			  (mode . TeX-mode)
			  (mode . LaTeX-mode)))
	       ("MAGIT"  (or
			  (mode . magit-mode)
			  (mode . magit-branch-manager-mode)
			  (mode . magit-commit-mode)
			  (mode . magit-diff-mode)
			  (mode . magit-log-mode)
			  (mode . magit-process-mode)
			  (mode . magit-status-mode)
			  (mode . magit-wazzup-mode)
			  (mode . git-commit-mode)))
	       ))))
;; Group for the remaning unclassified buffers.
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))


;;; The following two need to travel together in this sequence 2014-02-24
;;
;;; ibuffer-git to add git support
(require 'ibuffer-git)
;; Choose the column width for the long status
;; (setq ibuffer-git-column-length 8)	; default is 8.
;; git-status-mini (git-status 8 8 :left) are defined. Add to ibuffer-formats
;;
;; Modify the default ibuffer-formats	; git support and size formatting
;; http://unix.stackexchange.com/questions/35830/change-column-width-in-an-emacs-ibuffer-on-the-fly
(setq ibuffer-formats
      '((mark modified read-only	; Three flags without spaces in between.
	      " "			;
	      (name 18 18 :left :elide)	; Buffer name
	      " "			;
	      git-status-mini		; ibuffer-git short status
	      " "			;
	      (size-h 9 -1 :right)	; size-h defined at the end
	      ;;(size 9 -1 :right)	; original size in bytes
	      " "			;
	      (mode 10 10 :left :elide)	; Mode
	      " "
	      filename-and-process)))
;;
;;; Human readable buffer sizes
;; Use human readable "Size" column instead of original one 2014-01-15
;; http://www.emacswiki.org/emacs/IbufferMode#toc12
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))
;;


;;; reveal-in-finder.el 2014-02-05
;; Mac-only configuration
(when (eq system-type 'darwin)
  ;; https://github.com/kaz-yos/elisp
  (require 'reveal-in-finder)
  ;; autoload test
  ;; (autoload 'reveal-in-finder "reveal-in-finder")
  (global-set-key (kbd "C-c z") 'reveal-in-finder)
  )



;;; tempbuf.el	Auto-delete unused idle buffers such as dired
;; http://www.emacswiki.org/emacs/tempbuf.el
(require 'tempbuf)
;; No message
(setq tempbuf-kill-message nil)
;; (add-hook 'find-file-hooks		'turn-on-tempbuf-mode)	; All idle unedited files closed
(add-hook 'help-mode-hook		'turn-on-tempbuf-mode)	; Idle help closed
(add-hook 'dired-mode-hook		'turn-on-tempbuf-mode)	; Idle dired closed
;;(add-hook 'ess-help-mode-hook		'turn-on-tempbuf-mode)	; Idle ESS help closed
(add-hook 'completion-list-mode-hook	'turn-on-tempbuf-mode)	; Idle completion closed
(add-hook 'Snippet-mode-hook		'turn-on-tempbuf-mode)  ; Idle Snippets closed
(add-hook 'Custom-mode-hook		'turn-on-tempbuf-mode)	; Idle M-x customize closed
(add-hook 'fundamental-mode-hook 'turn-on-tempbuf-mode)	; Idle auto-install closed. Not working
(add-hook 'comint-mode-hook		'turn-on-tempbuf-mode)  ; 2013-09-09 for LaTeX inf. process
(add-hook 'latex-math-preview-expression-mode-hook 'turn-on-tempbuf-mode) ; 2013-09-09
;;
;; magit related modes
(add-hook 'magit-branch-manager-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'magit-commit-mode-hook	'turn-on-tempbuf-mode)
(add-hook 'magit-diff-mode-hook		'turn-on-tempbuf-mode)
(add-hook 'magit-log-mode-hook		'turn-on-tempbuf-mode)
(add-hook 'magit-process-mode-hook	'turn-on-tempbuf-mode)
(add-hook 'magit-status-mode-hook	'turn-on-tempbuf-mode)
(add-hook 'magit-wazzup-mode-hook	'turn-on-tempbuf-mode)
;;
;; VC related. VC is not used
;; (add-hook 'vc-annotate-mode-hook	'turn-on-tempbuf-mode)	; Idle VC annotate closed
;; (add-hook 'log-view-mode-hook		'turn-on-tempbuf-mode)	; Idle VC change log closed
;; (add-hook 'diff-mode-hook		'turn-on-tempbuf-mode)	; Idle VC diff closed
