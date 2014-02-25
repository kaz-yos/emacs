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
