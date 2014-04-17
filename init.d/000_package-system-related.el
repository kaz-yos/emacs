;;; package.el and repositories
;; THIS HAS TO COME BOFORE init-loader.el (installed via package.el)
;; http://www.emacswiki.org/emacs/ELPA
(require 'package)
;;
;; Load Emacs Lisp packages, and activate them.
;; (package-initialize)
;;
;; MELPA repository
;; http://melpa.milkbox.net/#installing
;; http://melpa.milkbox.net/#/getting-started
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; Marmalade repository (not active)
;; http://www.emacswiki.org/emacs/Marmalade
;; http://qiita.com/items/e81fca7a9797fe203e9f
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;; org mode repository
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;
;; Refresh contents  if no package-archive-contents available
;; http://stackoverflow.com/questions/14836958/updating-packages-in-emacs
(when (not package-archive-contents)
  (package-refresh-contents))
;;
;; Need to be initialized.
(package-initialize)


;;; melpa.el (Obsolete in 24.4. Kept for backward compatibility)
;; Installation:
;; (progn
;;   (switch-to-buffer
;;    (url-retrieve-synchronously
;;     "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
;;   (package-install-from-buffer  (package-buffer-info) 'single))
;;
;; Not necessary in 24.4
;; https://github.com/milkypostman/melpa/issues/1031
;; Load melpa.el if the stable version 2014-03-02
(when (equal emacs-version "24.3.1")
  ;; in "24.3.1" only
  (add-to-list 'load-path "~/.emacs.d/plugins/melpa-20131205.15")
  (require 'melpa))


;;; el-get.el package system 2013-02-26
;; https://github.com/dimitri/el-get
;; The load-path is configured at the top of init.el.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(el-get 'sync)
