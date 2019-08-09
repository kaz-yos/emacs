;;; package.el and repositories
(require 'package)
;;
;;;  ELPA
;; https://elpa.gnu.org/packages/
;; https://www.reddit.com/r/emacs/comments/ccrxk9/elpa_down/
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")))
;;
;;;  MELPA repository
;; http://melpa.milkbox.net/#installing
;; http://melpa.milkbox.net/#/getting-started
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;;;  MELPA Stable
;; http://stable.melpa.org/#/getting-started
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;
;; Marmalade repository (not active)
;; http://www.emacswiki.org/emacs/Marmalade
;; http://qiita.com/items/e81fca7a9797fe203e9f
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;;;  org mode repository
;; http://orgmode.org/elpa.html
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
;;
;;;  local-melpa-stan
(let ((local-melpa-stan
       "~/Documents/programming/emacs-lisp-repos/stan-mode/local-melpa/packages"))
  (when (file-exists-p local-melpa-stan)
    (add-to-list 'package-archives
                 `("local-melpa-stan" . ,local-melpa-stan) t)))
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
;; This is no longer necessary in emacs 27.
;; https://github.com/jkitchin/scimax/issues/194
(when (version< emacs-version "27.0")
  (package-initialize t))


;;;
;;; use-package.el
;; https://github.com/jwiegley/use-package
;; emacs-use-package-fast
;; https://github.com/nilcons/emacs-use-package-fast
;;
;; ;; Package-Requires: ((emacs "24.3") (bind-key "2.4"))
;;
;; (require FEATURE &optional FILENAME NOERROR)
(require 'use-package)
;;
;; Whether to report about loading and configuration details.
(setq use-package-verbose t)
;;
;; Automatic installation when packages not installed. (Don't use many errors).
;; https://github.com/jwiegley/use-package#for-packageel-users
;; (setq use-package-always-ensure t)
