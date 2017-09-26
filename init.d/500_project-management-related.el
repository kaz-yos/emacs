;;;
;;; PROJECTILE-RELATED

;;;; projectile.el
;; https://github.com/bbatsov/projectile
;; https://github.com/bbatsov/projectile#interactive-commands
;; https://projectile.readthedocs.io/en/latest/usage/
(use-package projectile
  :commands (;; This is used elsewhere
             projectile-project-root
             projectile-switch-project
             projectile-commander
             projectile-project-p)
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file))
  ;;
  :config
  ;; Simpler lighter
  (setq projectile-mode-line '(:eval
                               (format " [%s]"
                                       (projectile-project-name))))
  ;; A static lighter with no evaluation should not be slow over TRAMP.
  ;; https://github.com/bbatsov/projectile/issues/657
  ;; https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh
  ;; (setq projectile-mode-line " ")
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
  ;; Load saved projects from `projectile-known-projects-file'.
  ;; Also set `projectile-known-projects'.
  (projectile-load-known-projects)
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
  (setq projectile-find-dir-includes-top-level t)
  ;;
;;;  helm-projectile.el
  ;; http://tuhdo.github.io/helm-projectile.html
  (use-package helm-projectile
    :bind ("C-M-z" . helm-projectile))
  ;;
;;;  counsel-projectile.el
  ;; https://github.com/ericdanan/counsel-projectile
  (use-package counsel-projectile
    :bind (:map projectile-command-map
                ("SPC" . counsel-projectile))
    :config
    (counsel-projectile-on)))
