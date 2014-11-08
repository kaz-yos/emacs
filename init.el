;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el for emacs 24.3.90.1 on Mac OS X 10.9
;; Downloaded in the binary form from http://emacsformacosx.com/builds
;; Reference: http://sakito.jp/emacs/emacs24.html#ime
;; Now managed with init-loader.el: https://github.com/emacs-jp/init-loader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Configure load-path (load-path should only be defined in init.el)
;;
;; http://emacswiki.org/emacs/LoadPath	; Recursive for elpa directory
;; This is NECESSARY if init-loader.el is required before package.el package-initialize.
(let ((default-directory "~/.emacs.d/elpa/"))
       (normal-top-level-add-subdirs-to-load-path))
;;
;; el-get.el package system 2013-02-26 (additional configuration further below)
;; https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;;
;; Packages added by auto-install (put no subfolders)
(add-to-list 'load-path "~/.emacs.d/auto-install/")
;;
;; Packages added manually (intentionally not recursive)
(add-to-list 'load-path "~/.emacs.d/plugins/")
;;
;; Manually installed themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;;; init-loader.el
;; https://github.com/emacs-jp/init-loader
;; http://d.hatena.ne.jp/hiro_nemu/20140118/1390058851
;; http://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;; http://shibayu36.hatenablog.com/entry/20101229/1293624201
;;
;; This configuration is highly dependent on init-loader.el!
;;
;; Load init-loader.el
(require 'init-loader)
;; Always report errors and logs (default t)
(setq init-loader-show-log-after-init t)
;; Report errors after initialization only if there is any
;; (if (not (equal (init-loader-error-log) ""))
;;     (init-loader-show-log))
;; Load appropriate files in this directory
(init-loader-load "~/.emacs.d/init.d")
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

