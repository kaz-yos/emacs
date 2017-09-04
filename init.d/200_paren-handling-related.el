;;; Automatically close brackets and parentheses
;; http://www.emacswiki.org/emacs/ESSAutoParens
;; http://www.emacswiki.org/emacs/SkeletonPair

;;;
;;; paren.el
(use-package paren
  :config
  (show-paren-mode t))


;;;
;;; elec-pair.el
;; Electric Pair mode is a global minor mode.  When enabled, typing
;; an open parenthesis automatically inserts the corresponding
(use-package elec-pair
  :config
  (electric-pair-mode t)
  ;; If non-nil, skip char instead of inserting a second closing paren.
  ;; Better not to prevent overlapping ] before ]
  (setq electric-pair-skip-self nil))
