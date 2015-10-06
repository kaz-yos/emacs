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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; MELPA Stable
;; http://stable.melpa.org/#/getting-started
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;
;; Marmalade repository (not active)
;; http://www.emacswiki.org/emacs/Marmalade
;; http://qiita.com/items/e81fca7a9797fe203e9f
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;; org mode repository
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;
;; Refresh contents  if no package-archive-contents available
;; http://stackoverflow.com/questions/14836958/updating-packages-in-emacs
(when (not package-archive-contents)
  (package-refresh-contents))
;;
;; Some packages use defstruct, which is obsolete. Use cl-defstruct
(defalias 'defstruct 'cl-defstruct)
;;
;; Need to be initialized.
(package-initialize)


;;; el-get.el package system
;; https://github.com/dimitri/el-get
;;
;; The load-path is configured at the top of init.el.
;;
;;; Alternative Basic Setup with Installation via MELPA
;; https://github.com/dimitri/el-get#alternative-basic-setup-with-installation-via-melpa
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))
;;
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;
(el-get 'sync)
;;
;;; Packages installed via el-get
;; smartchr
;;
;; essh no longer on el-get as of 2015-10-05. Installed from emacswiki
;; moccur-edit no longer on el-get as of 2015-10-05. Installed from emacswiki
;; highlight-sexp no longer on el-get as of 2015-10-05. Installed from emacswiki
