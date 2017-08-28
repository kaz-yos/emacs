;;;
;;; PROJECTILE-RELATED

;;;; projectile.el
;; https://github.com/bbatsov/projectile
;; https://github.com/bbatsov/projectile#interactive-commands
;; https://projectile.readthedocs.io/en/latest/usage/
(use-package projectile
  ;; C-c p s should always work even before entering into a project
  :bind (("C-c p s" . projectile-switch-project))
  ;;
  :config
  ;; Simpler lighter
  ;; (setq projectile-mode-line '(:eval
  ;;                              (format " [%s]"
  ;;                                      (projectile-project-name))))
  ;; Static lighter with no evaluation
  (setq projectile-mode-line " ")
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
  ;;
  ;; Indexing
  ;; https://github.com/bbatsov/projectile#indexing-method
  ;; (setq projectile-indexing-method 'native)
  ;; Caching
  ;; https://github.com/bbatsov/projectile#caching
  (setq projectile-enable-caching t)
  ;; No ido use for completion.
  (setq projectile-completion-system 'default)
  ;; Switching
  ;; https://github.com/bbatsov/projectile#switching-projects
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-find-dir-includes-top-level t))


;;;; helm-projectile.el
;; http://tuhdo.github.io/helm-projectile.html
(use-package helm-projectile
  :after (projectile)
  :commands (helm-projectile)
  :bind ("C-M-z" . helm-projectile))


;;; counsel-projectile.el
;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :after (projectile)
  :bind (:map projectile-command-map
              ("SPC" . counsel-projectile))
  :config
  (counsel-projectile-on))
