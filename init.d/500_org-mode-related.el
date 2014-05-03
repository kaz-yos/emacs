;;; Org-mode related
;; Load org
(require 'org)
;;
;;;
;;; Reassign Keep the following key
(define-key org-mode-map (kbd "<C-tab>") 'other-window-or-split)
(define-key org-mode-map (kbd "<A-tab>") 'org-global-cycle)
