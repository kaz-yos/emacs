;;; projectile.el		; Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
;; https://github.com/bbatsov/projectile#interactive-commands
(require 'projectile)
;; Enable Projectile globally
(projectile-global-mode)
;; Simpler lighter
(setq projectile-mode-line-lighter " Project")
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
(setq projectile-switch-project-action 'projectile-find-file)	; default
;; (setq projectile-switch-project-action 'projectile-dired)
(setq projectile-find-dir-includes-top-level t)
;; 
;;
;;; helm-projectile.el		; Helm plugin for projectile
;; C-c p h for helm-projectile
(require 'helm-projectile)
;; C-M-z for helm-projectile
(global-set-key (kbd "C-M-z") 'helm-projectile)



;;; project-explorer.el		; A project explorer sidebar
;; https://github.com/sabof/project-explorer
;; (require 'project-explorer)


;;; eproject.el			; Assign files to projects, programatically
;; http://www.emacswiki.org/emacs/eproject
;; https://github.com/jrockway/eproject
;; http://d.hatena.ne.jp/yuheiomori0718/20111227/1324995109
;; http://d.hatena.ne.jp/yuheiomori0718/20111228/1325076537
;; (require 'eproject)


;;; prosjekt.el			; a software project tool for emacs
;; https://github.com/abingham/prosjekt
