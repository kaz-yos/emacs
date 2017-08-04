;;; package.el and repositories
(require 'package)
;;
;;
;;;  MELPA repository
;; http://melpa.milkbox.net/#installing
;; http://melpa.milkbox.net/#/getting-started
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;;;  MELPA Stable
;; http://stable.melpa.org/#/getting-started
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;
;; Marmalade repository (not active)
;; http://www.emacswiki.org/emacs/Marmalade
;; http://qiita.com/items/e81fca7a9797fe203e9f
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;;;  org mode repository
;; http://orgmode.org/elpa.html
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;;
;; Refresh contents  if no package-archive-contents available
;; http://stackoverflow.com/questions/14836958/updating-packages-in-emacs
;; (when (not package-archive-contents)
;;   (package-refresh-contents))
;;
;; Some packages use defstruct, which is obsolete. Alias cl-defstruct to avoid errors.
;; 2016-01-09 latex-math-preview still depends on defstruct, and gives an error w/o it.
(defalias 'defstruct 'cl-defstruct)
;;
;; Load Emacs Lisp packages, and activate them.
;; (package-initialize &optional NO-ACTIVATE)
;; If optional arg NO-ACTIVATE is non-nil, don’t activate packages.
;; https://emacs.stackexchange.com/questions/16831/using-package-el-to-install-and-update-but-use-package-for-loading-and-configuri
(package-initialize t)


;;;
;;; use-package.el for clean package loading
;; https://github.com/jwiegley/use-package
;; emacs-use-package-fast
;; https://github.com/nilcons/emacs-use-package-fast
;;
;; (require FEATURE &optional FILENAME NOERROR)
(require 'use-package)
;; Whether to report about loading and configuration details.
(setq use-package-verbose t)
;;
;; Automatic installation when packages not installed. (Don't use many errors).
;; https://github.com/jwiegley/use-package#for-packageel-users
;; (setq use-package-always-ensure t)


;;;
;;; el-get.el package system
;; https://github.com/dimitri/el-get
;;
;; The load-path is configured at the top of init.el.
;;
;; Alternative Basic Setup with Installation via MELPA
;; https://github.com/dimitri/el-get#alternative-basic-setup-with-installation-via-melpa
(use-package el-get
  ;; Deferred loading unless called explicitly.
  :commands (el-get-list-packages)
  ;;
  :config
  (add-to-list 'el-get-recipe-path (concat user-emacs-directory
                                           "el-get-user/recipes"))
  (el-get 'sync))
;;
;;;  Packages installed via el-get. Explicitly specify their paths.
;;;   smartchr
