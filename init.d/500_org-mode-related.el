;;; Org-mode related
;; Load org
(use-package org
  :mode ("\\.org" . org-mode)
  :config
  ;; Reassign Keep the following key
  (define-key org-mode-map (kbd "<C-tab>") 'other-window-or-split)
  (define-key org-mode-map (kbd "<A-tab>") 'org-global-cycle))
