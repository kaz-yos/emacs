;;; dired-plus 2014-02-04
;; http://www.emacswiki.org/emacs/DiredPlus
;; http://ergoemacs.org/emacs/emacs_diredplus_mode.html
(require 'dired+)
;;
;;  Hide/Show Details
;;  -----------------
;;  Starting with Emacs 24.4, listing details are hidden by default.
;;  Use `(' anytime to toggle this hiding.  You can use option
;;  `diredp-hide-details-initially-flag' to change the default/initial
;;  state.  See also option `diredp-hide-details-propagate-flag'.
;;
;;  If you have an Emacs version older than 24.4, you can use library
;;  `dired-details+.el' (plus `dired-details.el') to get similar
;;  behavior.
;;
;; Show details by default in 24.4 (does not work here? configure in dired config)
;; (setq diredp-hide-details-initially-flag nil)
;;
;; Other dired inherit the current setting
;; (setq diredp-hide-details-propagate-flag t)
;;


;;; dired-subtree.el
;; https://github.com/Fuco1/dired-hacks/blob/master/dired-subtree.el
(require 'dired-subtree)
;;
;; http://rubikitch.com/2014/12/22/dired-subtree/
;; i is subtree
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
;; tab folding
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-remove)
;; C-x n n for narrowing
(define-key dired-mode-map (kbd "C-x n n") 'dired-subtree-narrow)
;; ^ for dired-subtree
(defun dired-subtree-up-dwim (&optional arg)
  "Go to parent dir or open parent dir"
  (interactive "p")
  (or (dired-subtree-up arg)
      (dired-up-directory)))
(define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)


;;; runner.el
;; https://github.com/thamer/runner
;; http://rubikitch.com/2015/01/13/runner-3/
(require 'runner)
(define-key dired-mode-map (kbd "C-c !") 'runner-add-extension)


;;; stripe-buffer.el
;; Turned off. Need decent color settings
;; https://github.com/sabof/stripe-buffer
(require 'stripe-buffer)
;; (add-hook 'dired-mode-hook 'stripe-listify-buffer)
;; (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
