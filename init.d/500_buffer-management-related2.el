;;; Buffer-releated configurations with external dependencies  -*- lexical-binding: t; -*-


;;;
;;; Swap buffers with C-x /
;; http://stackoverflow.com/questions/1510091/with-emacs-how-do-you-swap-the-position-of-2-windows
(defun swap-buffer ()
  "Swap buffers between current and other window"
  (let* ((buffer-a (current-buffer))
         ;; Get second window
         (window-b (cadr (window-list)))
         ;; GEt buffer in second window
         (buffer-b (window-buffer window-b)))
    ;; Swapping
    (set-window-buffer window-b buffer-a)
    (switch-to-buffer buffer-b)
    (other-window 1)))
;;
(defun swap-buffer-plus ()
  "Swap buffers among windows

When there is only one window, split it into two.
When there are two windows, swap the buffers.
When there are three or more windows, call ace-swap-window.
Dependency: ace-swap-window"
  (interactive)
  (pcase (count-windows)
    (`1 (display-buffer (other-buffer)))
    (`2 (swap-buffer))
    (_  (ace-swap-window))))
;;
(global-set-key (kbd "C-x /") 'swap-buffer-plus)


;;;
;;; reveal-in-finder.el
;; Mac-only configuration
(when (eq system-type 'darwin)

  ;; Add path to developmental repo
  ;; (when (file-exists-p "~/Documents/programming/emacs-lisp-repos/reveal-in-osx-finder")
  ;;   (add-to-list 'load-path "~/Documents/programming/emacs-lisp-repos/reveal-in-osx-finder"))

  ;; https://github.com/kaz-yos/elisp
  (require 'reveal-in-osx-finder)
  ;; autoload test
  ;; (autoload 'reveal-in-finder "reveal-in-finder")
  (global-set-key (kbd "C-c z") 'reveal-in-osx-finder)
  (define-key TeX-mode-map (kbd "C-c z z") 'reveal-in-osx-finder))


;;;
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


;;;
;;; ibuffer-git.el
;; The following two configurations need to travel together in this sequence 2014-02-24
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
