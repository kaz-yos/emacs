;;; smart-mode-line.el		; A color coded smart mode-line.
;; https://github.com/Bruce-Connor/smart-mode-line/
;; https://github.com/Bruce-Connor/smart-mode-line/blob/master/smart-mode-line.el
;; 
;; Theme: This can be 'dark, 'light or 'respectful.
;; ALL COLORS ARE CUSTOMIZABLE! `sml/customize-faces'
(setq sml/theme 'light)
;; cutom background colors
(setq sml/inactive-background-color "gray40")
(setq sml/active-background-color "gray80")
;;
;; Load
(require 'smart-mode-line)
(sml/setup)
;;
;; https://github.com/mgalgs/.emacs.d/blob/master/smart-mode-line-setup.el
;; Shortening
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 25)
(setq sml/mode-width 'full)
;;
;; modes to hide
(add-to-list 'sml/hidden-modes " ICY")
(add-to-list 'sml/hidden-modes " Icy")
(add-to-list 'sml/hidden-modes " SliNav")
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " Projectile")
;;
;;  3. `sml/replacer-regexp-list'
;;   This variable is a list of (REGEXP REPLACEMENT) that is used
;;   to parse the path. The replacements are applied
;; Here go some more useful examples:
;;
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Projects/In-Development/" ":ProjDev:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/" ":Work:"))
;;
;;     ;; Added in the right order, they even work sequentially:
;;     (add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Git-Projects/" ":Git:"))
