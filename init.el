;; ~/.emacs.d/init.el
;; Managed with init-loader.el: https://github.com/emacs-jp/init-loader


;;;
;;; Emacs installation
;;;  macOS binary (stable and release candidates)
;; https://emacsformacosx.com
;;
;;;  macOS Homebrew emacs-plus (stable or develop)
;; https://github.com/d12frosted/homebrew-emacs-plus
;; https://github.com/syl20bnr/spacemacs#macos (Recommended for Spacemacs)
;; Homebrew formulae do not build "proper" `.app` (including all elements).
;; $ brew tap d12frosted/emacs-plus
;; $ brew install emacs-plus --without-spacemacs-icon --HEAD # for the development version
;; $ brew linkapps emacs-plus # Warning: `brew linkapps` has been deprecated
;;
;;;  macOS Homebrew Cask (stable, pre-test, and old nightly)
;; http://wikemacs.org/wiki/Installing_Emacs_on_OS_X
;; $ brew cask search emacs # to check what's available.
;;
;;;  macOS source install
;; https://www.emacswiki.org/emacs/EmacsForMacOS#toc4
;; http://stuff-things.net/2015/10/13/building-emacs-on-os-x-el-capitan/
;; $ cd src
;; $ git clone git://git.savannah.gnu.org/emacs.git
;; $ cd emacs
;; $ make configure
;; $ ./configure --with-ns
;; $ make install
;; $ open -R nextstep/Emacs.app # to find the .app.
;;
;;;  Windows binary by Vincent Goule (stable)
;; https://vigou3.github.io/emacs-modified-windows/


;;;
;;; No garbage collection during loading.
(setq gc-cons-threshold most-positive-fixnum)


;;; Configure load-path (load-path should only be defined in init.el)
;;
;; http://emacswiki.org/emacs/LoadPath	; Recursive for elpa directory
;; This is NECESSARY if init-loader.el is required before package.el package-initialize.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
;; Whether to activate installed packages when Emacs starts.
(setq package-enable-at-startup nil)

;; Recursively add all subdirectories of ‘default-directory’ to ‘load-path’.
(let ((default-directory (concat user-emacs-directory "elpa/")))
  (normal-top-level-add-subdirs-to-load-path))
;;
;; Packages added by auto-install (put no subfolders)
(add-to-list 'load-path (concat user-emacs-directory "auto-install/"))
;;
;; Packages added manually (intentionally not recursive)
(add-to-list 'load-path (concat user-emacs-directory "plugins/"))
;;
;; Configure the custom theme directory
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html#Creating-Custom-Themes
(setq custom-theme-directory (concat user-emacs-directory "themes/"))


;;;
;;; package.el and repositories
(require 'package)
;; http://melpa.milkbox.net/#/getting-started
;; Change repository settings depending on gnutls availability.
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (protocol (if no-ssl "http" "https")))
  ;;
  (when no-ssl (warn "
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;;
;;;  ELPA
  ;; https://elpa.gnu.org/packages/
  (setq package-archives (list (cons "gnu" (concat protocol "://elpa.gnu.org/packages/"))))
  ;;
;;;  Non-GNU ELPA
  ;; https://elpa.nongnu.org
  (add-to-list 'package-archives (cons "nongnu" (concat protocol "://elpa.nongnu.org/nongnu/")))
  ;;
;;;  MELPA repository
  ;; http://melpa.milkbox.net/#installing
  ;; http://melpa.milkbox.net/#/getting-started
  (add-to-list 'package-archives (cons "melpa" (concat protocol "://melpa.org/packages/")) t)
  ;;
  )
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
;; Bootstrap use-package
;; https://github.com/jwiegley/use-package/issues/219
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;
;; (require FEATURE &optional FILENAME NOERROR)
(require 'use-package)
;;
;; Whether to report about loading and configuration details.
(setq use-package-verbose t)
;;
;; Automatic installation when packages not installed.
;; This fails by trying to install packages by the wrong names
;; based on what comes after each `use-package' call.
;; (setq use-package-always-ensure t)


;;;
;;; init-loader.el
;; https://github.com/emacs-jp/init-loader
;; http://d.hatena.ne.jp/hiro_nemu/20140118/1390058851
;; http://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;; http://shibayu36.hatenablog.com/entry/20101229/1293624201
;;
(use-package init-loader
  :ensure t
  :config
  ;; Always report errors and logs (default t)
  (setq init-loader-show-log-after-init t)
  ;; Load appropriate files in this directory
  (init-loader-load (concat user-emacs-directory "init.d"))
  ;; Configuration files tracked elsewhere
  (let ((dir (concat user-emacs-directory "private")))
    (when (file-exists-p dir)
      (init-loader-load dir)))
  ;;
  ;; Note that not all files in the directory are loaded.  Each file is
  ;; examined that if it is a .el or .elc file and, it has a valid name
  ;; specified by `init-loader-default-regexp' or it is a platform
  ;; specific configuration file.
  ;;
  ;; By default, valid names of configuration files start with two
  ;; digits.  For example, the following file names are all valid:
  ;;     00_util.el
  ;;     01_ik-cmd.el
  ;;     21_javascript.el
  ;;     99_global-keys.el
  ;;
  ;; Files are loaded in the lexicographical order.  This helps you to
  ;; resolve dependency of the configurations.
  ;;
  ;; A platform specific configuration file has a prefix corresponds to
  ;; the platform.  The following is the list of prefixes and platform
  ;; specific configuration files are loaded in the listed order after
  ;; non-platform specific configuration files.
  ;;
  ;; Platform   Subplatform        Prefix         Example
  ;; ------------------------------------------------------------------------
  ;; Windows                       windows-       windows-fonts.el
  ;;            Meadow             meadow-        meadow-commands.el
  ;; ------------------------------------------------------------------------
  ;; Mac OS X   Carbon Emacs       carbon-emacs-  carbon-emacs-applescript.el
  ;;            Cocoa Emacs        cocoa-emacs-   cocoa-emacs-plist.el
  ;; ------------------------------------------------------------------------
  ;; GNU/Linux                     linux-         linux-commands.el
  ;; ------------------------------------------------------------------------
  ;; All        Non-window system  nw-            nw-key.el
  ;;
  ;; If `init-loader-byte-compile' is non-nil, each configuration file
  ;; is byte-compiled when it is loaded.  If you modify the .el file,
  ;; then it is recompiled next time it is loaded.
  ;;
  ;; Loaded files and errors during the loading process are recorded.
  ;; If `init-loader-show-log-after-init' is non-nil, the record is
  ;; shown after the overall loading process.  You can do this manually
  ;; by M-x init-loader-show-log.
  )
