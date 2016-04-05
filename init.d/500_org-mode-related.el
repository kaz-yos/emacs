;;; Org-mode related
;; Load org
(use-package org
  :mode ("\\.org" . org-mode)
  :config
  ;; Reassign Keep the following key
  (define-key org-mode-map (kbd "<C-tab>") 'other-window-or-split)
  (define-key org-mode-map (kbd "<A-tab>") 'org-global-cycle)
  ;; This breaks other behaviors?
  ;; (define-key org-mode-map (kbd "<return>") 'org-meta-return)
  ;;
  ;; Linewrap in Org-mode of Emacs
  ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs
  (setq org-startup-truncated nil)
  (define-key org-mode-map "\M-q" 'toggle-truncate-lines)
  ;;
  ;; Do not fold at startup
  (setq org-startup-folded nil)
  ;; Indent at startup
  (setq org-startup-indented t)
  ;;
  ;; Beamer presentations using the new export engine
  ;; http://orgmode.org/worg/exporters/beamer/ox-beamer.html
  (require 'ox-beamer)
  ;; Pretty bullets
  ;; https://github.com/sabof/org-bullets
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
