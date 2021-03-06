;;; 500_project-management-related.el ---            -*- lexical-binding: t; -*-

;;;
;;; project.el
;; Part of emacs
(use-package project
  :commands (project-switch-project)
  :config)


;;;
;;; projectile.el
;; https://github.com/bbatsov/projectile
;; https://github.com/bbatsov/projectile#interactive-commands
;; https://projectile.readthedocs.io/en/latest/usage/
(use-package projectile
  :ensure t
  :demand t
  :diminish "Prjt"
  :commands (;; These commands are used elsewhere.
             projectile-project-root
             projectile-switch-project
             projectile-commander
             projectile-project-p
             projectile-project-name)
  :bind-keymap  ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
         ("SPC" . counsel-projectile))
  ;;
  :config
  (setq projectile-keymap-prefix (kbd "C-c p"))
  ;; Also use another prefix.
  ;; https://github.com/bbatsov/projectile/issues/991
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
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
  ;;
  ;; M-x profiler-start, M-x profiler-report if opening a file is slow.
  ;; https://www.murilopereira.com/how-to-open-a-file-in-emacs/#opening-a-file
  ;;
  ;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile
  ;; https://github.com/jonEbird/dotfiles/blob/master/.emacs.d/my_configs/email_config.el#L113
  ;; `projectile-globally-ignored-modes'
  ;; A list of regular expressions for major modes ignored by projectile.
  ;; If a buffer is using a given major mode, projectile will ignore
  ;; it for functions working with buffers.
  (add-to-list 'projectile-globally-ignored-modes "mu4e.*")
  ;;
  ;; Enable Projectile globally
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
;;;  counsel-projectile.el
  ;; https://github.com/ericdanan/counsel-projectile
  (use-package counsel-projectile
    :ensure t
    :demand t
    :config
    ;; Turn on mode
    ;; https://github.com/ericdanan/counsel-projectile#upgrading-from-previous-version
    (counsel-projectile-mode +1)))
