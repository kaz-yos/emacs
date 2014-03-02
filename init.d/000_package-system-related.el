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


;;; 24.4 work around
(when (not (equal emacs-version "24.3.1"))
  ;; https://github.com/LiaoPengyu/emacs.d/commit/fc7a6fedb5c496c613d6d19a4fcdab18b36a8d87
  ;; https://github.com/LiaoPengyu/emacs.d/blob/fc7a6fedb5c496c613d6d19a4fcdab18b36a8d87/init-elpa.el
;;; Add support to package.el for pre-filtering available packages
  (defvar package-filter-function nil
    "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

  (defadvice package--add-to-archive-contents
    (around filter-packages (package archive) activate)
    "Add filtering of available packages using `package-filter-function', if non-nil."
    (when (or (null package-filter-function)
	      (funcall package-filter-function
		       (car package)
		       (funcall (if (fboundp 'package-desc-version)
				    'package--ac-desc-version
				  'package-desc-vers)
				(cdr package))
		       archive))
      ad-do-it))
  )
;;
;;
;; Need to be initialized here after work around.
(package-initialize)


;;; melpa.el (will be obsolete in 24.4
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
(if (equal emacs-version "24.3.1")
    ;; t
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
