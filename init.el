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
;; $ brew install emacs-plus --with-natural-title-bar --HEAD # for the development version
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


;; Find what causes a package to be loaded.
;; (eval-after-load "hydra"
;;   '(debug))

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

(let ((default-directory (concat user-emacs-directory "elpa/")))
  (normal-top-level-add-subdirs-to-load-path))
;;
;; el-get.el package system 2013-02-26 (additional configuration further below)
;; https://github.com/dimitri/el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get/"))
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

;;; initchart.el start up process visualizer
;; (require 'initchart)
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)

;;; init-loader.el
;; https://github.com/emacs-jp/init-loader
;; http://d.hatena.ne.jp/hiro_nemu/20140118/1390058851
;; http://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;; http://shibayu36.hatenablog.com/entry/20101229/1293624201
;;
(require 'init-loader)
;; Always report errors and logs (default t)
(setq init-loader-show-log-after-init t)
;; Load appropriate files in this directory
(init-loader-load (concat user-emacs-directory "init.d"))
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
