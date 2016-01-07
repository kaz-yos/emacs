;;; projectile.el		; Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
;; https://github.com/bbatsov/projectile#interactive-commands
(use-package projectile
  :config
  ;; Simpler lighter
  (setq projectile-mode-line '(:eval
                               (format " Pj[%s]"
                                       (projectile-project-name))))
  ;; Enable Projectile globally
  ;; 2015-01-01 error *** Eval error ***  End of file during parsing
  ;; 2015-01-11 Solved by deleting .emacs.d/projectile-bookmarks.eld
  (projectile-global-mode)
  ;; C-c p s should always work even before entering into a project
  (global-set-key (kbd "C-c p s") 'projectile-switch-project)
  ;; Indexing
  ;; https://github.com/bbatsov/projectile#indexing-method
  ;; (setq projectile-indexing-method 'native)
  ;; Caching
  ;; https://github.com/bbatsov/projectile#caching
  (setq projectile-enable-caching t)
  ;; No ido use for completion (icicle can be used this way)
  (setq projectile-completion-system 'default)
  ;; Switching
  ;; https://github.com/bbatsov/projectile#switching-projects
  ;; (setq projectile-switch-project-action 'projectile-find-file)	; default
  ;; (setq projectile-switch-project-action 'helm-projectile)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-find-dir-includes-top-level t))

;;
;;
;;
;;; helm-projectile.el		; Helm plugin for projectile
;; http://tuhdo.github.io/helm-projectile.html
(use-package helm-projectile
  :commands (helm-projectile)
  :bind ("C-M-z" . helm-projectile))

;;
;;
;;; helm-projectile-all.el
;; https://github.com/spatz/helm-projectile-all
(use-package helm-projectile-all
  :commands (helm-projectile-all))
