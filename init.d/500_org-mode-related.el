;;; Org-mode related
;; Load org
(use-package org
  :mode ("\\.org" . org-mode)
  :config
  ;; Key bindings
  (define-key org-mode-map (kbd "<C-tab>") 'other-window-or-split)
  (define-key org-mode-map (kbd "<A-tab>") 'org-global-cycle)
  (define-key org-mode-map (kbd "A-s") 'org-latex-export-to-pdf)
  (define-key org-mode-map (kbd "A-C-s") 'org-beamer-export-to-pdf)
  ;; Backslash
  (define-key org-mode-map (kbd   ";") 'my-tex-insert-backslash)
  (define-key org-mode-map (kbd "A-;") 'my-tex-insert-semicolon)
  ;;
  (define-key org-mode-map (kbd "s-b") 'helm-org-in-buffer-headings)
  ;;
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
  ;; Indent at startup. This causes some strange behavior.
  ;; (setq org-startup-indented t)
  ;;
  ;;
  ;; Org-Bable
  ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-R.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (emacs-lisp . t)))
  ;;
  (setq org-src-fontify-natively t)
  ;;
  ;; Citation management
  ;; https://github.com/jkitchin/org-ref
  ;; https://github.com/jkitchin/org-ref/blob/master/org-ref.org
  ;; https://www.youtube.com/watch?v=2t925KRBbFc&nohtml5=False
  (require 'org-ref)
  ;;
  ;; Emacs Org-mode Bibtex Screencast
  ;; https://vimeo.com/99167082
  ;;
  ;; Beamer presentations using the new export engine
  ;; http://orgmode.org/worg/exporters/beamer/ox-beamer.html
  (require 'ox-beamer)
  ;;
  ;; Pretty bullets
  ;; https://github.com/sabof/org-bullets
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
