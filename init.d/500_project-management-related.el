;;;
;;; PROJECTILE-RELATED

;;;; projectile.el
;; https://github.com/bbatsov/projectile
;; https://github.com/bbatsov/projectile#interactive-commands
;; https://projectile.readthedocs.io/en/latest/usage/
(use-package projectile
  :config
  ;; Simpler lighter
  ;; (setq projectile-mode-line '(:eval
  ;;                              (format " [%s]"
  ;;                                      (projectile-project-name))))
  ;; Static lighter with no evaluation
  (setq projectile-mode-line " Prj")
  ;;
  ;; Machine specific cache files
  (setq projectile-cache-file (concat user-emacs-directory
                                      "projectile-cache"
                                      "_"
                                      (system-name-sans-domain)))
  (setq projectile-known-projects-file (concat user-emacs-directory
                                               "projectile-bookmarks"
                                               "_"
                                               (system-name-sans-domain)
                                               ".eld"))
  ;;
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

;;;; helm-projectile.el
;; http://tuhdo.github.io/helm-projectile.html
(use-package helm-projectile
  :commands (helm-projectile)
  :bind ("C-M-z" . helm-projectile))
