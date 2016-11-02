;;; Buffere-related configurations  -*- lexical-binding: t; -*-


;;;
;;; Unique buffer names
;; ;; rubikitch book p84
;; ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;; (require 'uniquify)
;; ;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)	; rubikitch
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;;
;;; auto-revert buffers globally
;; Active in all buffers
(setq global-auto-revert-mode nil)
;; Even in non-file buffers
(setq global-auto-revert-non-file-buffers t)
;; VC status change is also captured
(setq auto-revert-check-vc-info t)
;; No ARev in mode-line
(setq auto-revert-mode-text "")



;;;
;;; ibuffer.el
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
;;
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
	       ("ELISP"	(or
			 (mode . emacs-lisp-mode)
			 (mode . list-mode)
			 (name . "^\\*ielm")))
	       ("CLOJURE" (or
			   (mode . clojure-mode)
			   (name . "^\\*cider-")
			   (name . "^\\*nrepl-")))
	       ("SLIME" (or
			 (mode . lisp-mode)
			 (name . "^\\*slime")
			 (name . "*inferior-lisp*")))
	       ("SCHEME" (or
			  (mode . scheme-mode)
			  (mode . inferior-scheme-mode)
			  (mode . geiser-repl-mode)))
	       ("HASKELL" (or
                           (mode . haskell-mode)
                           (mode . inferior-haskell-mode)))
	       ("PYTHON" (or
			  (mode . python-mode)
			  (mode . inferior-python-mode)
                          (mode . ein:notebooklist-mode)
                          (mode . ein:notebook-multilang-mode)))
	       ("ML" (or
                      (mode . sml-mode)
                      (mode . inferior-sml-mode)))
               ("RUBY" (or
                      (mode . ruby-mode)
                      (mode . inf-ruby-mode)))
	       ("SHELL"  (or
			  (mode . sh-mode)
			  (mode . shell-mode)
			  (mode . ssh-mode)
			  (mode . eshell-mode)))
	       ("SQL"  (or
			(mode . sql-mode)
			(mode . sql-interactive-mode)))
	       ("TeX"    (or
			  (mode . TeX-mode)
			  (mode . LaTeX-mode)))
	       ("MAGIT"  (or
			  (mode . magit-mode)
			  (name . "^\\*magit")))))))
;; Group for the remaning unclassified buffers.
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
