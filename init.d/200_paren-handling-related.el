;;; Automatically close brackets and parentheses
;; http://www.emacswiki.org/emacs/ESSAutoParens
;; http://www.emacswiki.org/emacs/SkeletonPair
;; 20130426 enable skeleton-pair in iESS mode only.
;; Work around for problem with inserting before a parenthesis.
;; In other mode the default electric-pair-mode works good
;;

;;; Show-Paren mode
;; enable
(show-paren-mode t)				


;; Define a function for local activation
;; (defun my-skeleton-pair-enable ()
;;   (setq skeleton-pair t)		; on 20130426
;;   (setq skeleton-pair-on-word t)	; on 20130426
;;   (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;;   (local-set-key (kbd "[") 'skeleton-pair-insert-maybe) ; NOT Better handled with smartchr.el
;;   (local-set-key (kbd "\{") 'skeleton-pair-insert-maybe) ; This does not work? added \ 20130426
;;   (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;;   (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)	; Not useful for R, or lisp
;;   (local-set-key (kbd "\`") 'skeleton-pair-insert-maybe)	; Not useful for R
;;   ;; (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)	; Not useful for R
;;   )
;;
;; (add-hook 'ess-mode-hook 'my-skeleton-pair-enable)
;; (add-hook 'inferior-ess-mode-hook 'my-skeleton-pair-enable)
;; Better to use them everywhere along with electric pair?? Started trying on 20130426
(setq skeleton-pair t)		; on 20130426
(setq skeleton-pair-on-word t)	; on 20130426
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe) ; NOT Better handled with smartchr.el
(global-set-key (kbd "\{") 'skeleton-pair-insert-maybe) ; This does not work?  added \ 20130426
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)	; Not useful for lisp
;; (global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)	; Not useful for R
;; (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)	; Not useful for R
;;
;;
;; electric-pair-mode to automatically close brackets (New in emacs 24.1)
;; http://www.emacswiki.org/emacs/AutoPairs
;; These alone cannot place () around words? Use with skeleton-pair 20130426
(electric-pair-mode t)			;
(setq electric-pair-skip-self nil)	; Do not prevent overlapping ] before ]
