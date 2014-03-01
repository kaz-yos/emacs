;;; project-explorer.el		; A project explorer sidebar
;; https://github.com/sabof/project-explorer
;; (require 'project-explorer)


;;; projectile.el		; Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
(require 'projectile)
;; Enable Projectile globally 
(projectile-global-mode)
;; https://github.com/bbatsov/projectile#indexing-method
(setq projectile-indexing-method 'native)
;; https://github.com/bbatsov/projectile#caching
(setq projectile-enable-caching t)


;;; eproject.el			; Assign files to projects, programatically
;; http://www.emacswiki.org/emacs/eproject
;; https://github.com/jrockway/eproject
;; http://d.hatena.ne.jp/yuheiomori0718/20111227/1324995109
;; http://d.hatena.ne.jp/yuheiomori0718/20111228/1325076537
;; (require 'eproject)

