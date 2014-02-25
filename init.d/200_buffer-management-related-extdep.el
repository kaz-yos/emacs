;;; Swap buffers with C-x /
;; http://stackoverflow.com/questions/1510091/with-emacs-how-do-you-swap-the-position-of-2-windows
(defun swap-buffer ()
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))
(global-set-key (kbd "C-x /") 'swap-buffer)		; Enabled for everywhere

;;; Unique buffer names
;; rubikitch book p84
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)	; rubikitch
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")


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
;; Group for the other buffers.
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
