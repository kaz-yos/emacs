;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el for cocoa emacs 24.3.1 (source with inline patch) on Mac OS X 10.8
;; Reference: http://sakito.jp/emacs/emacs24.html#ime
;; Now managed with init-loader.el: https://github.com/emacs-jp/init-loader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Additional load-paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; http://emacswiki.org/emacs/LoadPath	; Recursive for elpa directory
(let ((default-directory "~/.emacs.d/elpa/"))
       (normal-top-level-add-subdirs-to-load-path))
;;
;; MELPA	; Actual configuration below
;; http://melpa.milkbox.net/#installing
;;
;; el-get.el package system 2013-02-26 (additional configuration further below)
;; https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;;
;; Packages added by auto-install (Book by rubikitch p49) (no subfolders)
(add-to-list 'load-path "~/.emacs.d/auto-install/")
;;
;; Packages added manually (intentionally not recursive)
(add-to-list 'load-path "~/.emacs.d/plugins/")
;;
;; exec-path-from-shell.el to configure correct $PATH for external files
;; http://d.hatena.ne.jp/syohex/20130718/1374154709
;; check with (getenv "PATH")
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;;
;; OBSOLETE
;; System-specific configuration
;; http://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp
;; (when (eq system-type 'darwin)
;;   Mac only
;;   $PATH for external commands		; Necessary for AUCtex platex coordination
;;   http://emacswiki.org/emacs/EmacsApp
;;   http://rforge.org/2011/08/16/sane-path-variable-in-emacs-on-mac-os-x/
;;   You can use them to have the same PATH as .bashrc sets
;;   (if (not (getenv "TERM_PROGRAM"))
;;       (setenv "PATH"
;;   	      (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration without external .el file dependencies comes first ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Use Mac OS X system trash
;; http://www.masteringemacs.org/articles/2010/12/30/making-deleted-files-trash-can/
;; http://www.reddit.com/r/emacs/comments/iuyef/emacs_on_mac/
(when (eq system-type 'darwin)
  ;; Mac-only
  (setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs")
  )


;; History retained between sessions
;; Book by rubikitch p59
;; http://www.emacswiki.org/emacs/SaveHist
(savehist-mode 1)
;; saveplace
;; Remeber the cursor position in a file
;; http://www.emacswiki.org/emacs/SavePlace
;; http://git.sysphere.org/dotfiles/tree/emacs
(setq save-place-file "~/.emacs.d/emacs-places")		; save file within ~/.emacs.d
(setq-default save-place t)
(require 'saveplace)
;; y or n for yes or no
(defalias 'yes-or-no-p 'y-or-n-p)


;; Bookmarks
;; http://www.emacswiki.org/emacs/BookMarks#toc6
(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs.d/bookmarks")		; save file within ~/.emacs.d
(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))
;;
;; Key-bindings (not configured by default in 24.2) ; Does not work
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html
;; (global-set-key (kbd "C-x r m") 'bookmark-set)
;; (global-set-key (kbd "C-x r b") 'bookmark-jump)
;; (global-set-key (kbd "C-x r l") 'list-bookmarks)



;; No auto filling in text mode
;; http://tomikura.s2.xrea.com/linux/install/emacs.html
(setq fill-column 80)
(setq text-mode-hook '(lambda () (auto-fill-mode 0)))
;;(setq default-major-mode 'text-mode)	; Obsolete as of version 23.2
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html
(setq-default major-mode 'text-mode)


;; delay linum for speed
;; http://d.hatena.ne.jp/daimatz/20120215/1329248780
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))


;;; Miscellaneous configurations without external dependencies

;; Surround region
;; http://www.emacswiki.org/emacs/SurroundRegion
(defun surround (begin end open close)
  "Put OPEN at START and CLOSE at END of the region.
If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (when (string= close "")
    (setq close open))
  (save-excursion
    (goto-char end)
    (insert close)
    (goto-char begin)
    (insert open)))




;;; External dependencies starting here. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; el-get.el package system 2013-02-26
;; https://github.com/dimitri/el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")	; This is configured at the top.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(el-get 'sync)


;;; Package system and MELPA
;; THIS HAS TO COME BOFORE init-loader.el (installed via package.el)
;; http://www.emacswiki.org/emacs/ELPA
(require 'package)
;;
;; Load Emacs Lisp packages, and activate them.
(package-initialize)
;;
;; MELPA	;
;; http://melpa.milkbox.net/#installing
;; http://melpa.milkbox.net/#/getting-started
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; Marmalade (not active
;; http://www.emacswiki.org/emacs/Marmalade
;; http://qiita.com/items/e81fca7a9797fe203e9f
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;; org mode
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;
;; Refresh contents 2013-10-31 Manually running this fixed issue of not having available packages?
;; http://stackoverflow.com/questions/14836958/updating-packages-in-emacs
(when (not package-archive-contents)
  (package-refresh-contents))
;;
;; melpa.el 2013-10-31 it was at the end
(require 'melpa)


;;; Mac-only configuration
(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Spell checker (require aspell. No .el dependency)
  ;; http://blog.bungu-do.jp/archives/2426
  (setq ispell-program-name "/usr/local/bin/aspell")

  ;; reveal-in-finder.el 2014-02-05
  ;; https://github.com/kaz-yos/elisp
  (require 'reveal-in-finder)
  ;; autoload test
  ;; (autoload 'reveal-in-finder "reveal-in-finder")
  (global-set-key (kbd "C-c z") 'reveal-in-finder)

  ;; cmigemo (installed from Homebrew)
  ;; Used brew to install cmigemo
  ;; Used M-x list-package to install migemo.el (github)
  ;; Configured refering to: http://d.hatena.ne.jp/ground256/20111008/1318063872
  (require 'migemo)
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init)
  )


;;; M x Customize
;; M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#042028")
 '(background-mode dark)
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes (quote ("06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" "5bff694d9bd3791807c205d8adf96817ee1e572654f6ddc5e1e58b0488369f9d" "f5db04080a5133bc99721d680a11cf974d60d1df347b08841b43c3e97f52d3bf" "fe0a47cc3952fede574527a1c28ddf3a1af381fc1fb5843ca60d22e4c841011a" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "1a093e45e4c3e86fa5ad1f8003660e7cda4d961cd5d377cee3fee2dad2faf19b" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "465be5317c7d95a84e376e095c21242f4f2ad75692ed806dcbb6fe27078260f1" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "30f861ee9dc270afc2a9962c05e02d600c998905433c8b9211dc2b33caa97c51" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "1c1e6b2640daffcd23b1f7dd5385ca8484a060aec901b677d0ec0cf2927f7cde" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" "d971315c813b0269a86e7c5e73858070063016d9585492bd8d0f27704d50fee7" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "605080e40891cc991f53d3d9c79b427d18497d973a44fd12a86d2360429a6a3d" "865d6cb994f89c13b2d7e5961df4eabeea12494583c240c8fe9a788d0f4ee12c" "eacfc96fbe418c017f4a00fdde5d5029db8e2800a46251eb2174484fa431917e" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "d7f1c86b425e148be505c689fc157d96323682c947b29ef00cf57b4e4e46e6c7" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "8d584fef1225d72bfd32d7677ac7f281208140a2535ef0e9f46f0e76343f8aca" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "73b835431bdbc4e83a3b176a38ebb740fbac78aa2635e1d4827b3c8211e0bc99" "436dd3eb5ff5be80d2db88494b340fcf34dc70a715d19c5aa7b794b763ff0321" "e57e7b19da7b4cd0e5512d5e9bc20d31c9cf50112c462de15a76bce0ea3c5ef5" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "364a5e1aecdd0d24b70089050368851ea5ee593dc8cc6fb58cff1b8cfe88a264" default)))
 '(fci-rule-character-color "#202020")
 '(foreground-color "#708183")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(linum-format " %7i ")
 '(magit-diff-options nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111"))
;;  '(yas-trigger-key "TAB")	; deleted. obsolete variable? 2014-02-02
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "dark blue" :weight ultra-bold :height 2.0))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil))))


;;; helpers

;; minor-mode-hack.el
;; M-x show-minor-mode-map-priority to see minor mode priority
(require 'minor-mode-hack)

;; show-keys.el	; Show keys in a buffer as keys are typed.
;; http://www.youtube.com/watch?v=0cZ7szFuz18
;; https://github.com/AndreaCrotti/minimal-emacs-configuration
(require 'show-keys)

;; windresize for M-x windresize
;; M-x windresize, arrows, C-g for cancel, RET to save
(require 'windresize)



;;; e2wn		; Window management system
;; ;; e2wm minimal configuration (requires window-layout.el)
;; ;; http://d.hatena.ne.jp/kiwanami/20100528/1275038929
;; (require 'e2wm)
;; (global-set-key (kbd "M-+") 'e2wm:start-management)
;; ;;
;; ;; e2wn-R
;; ;; http://sheephead.homelinux.org/2011/03/15/6687/
;; ;;(require 'inlineR) automatically loaded
;; (require 'e2wm-R)


;;; polymode (alpha) 2014-02-21
;; https://github.com/vitoshka/polymode
;; Set load path (forked and pulled from my repo)
;; (setq load-path
;;       (append '("~/.emacs.d/plugins/polymode/"  "~/.emacs.d/plugins/polymode/modes")
;;               load-path))
;; ;;
;; ;; Activate necessary bundles
;; (require 'poly-R)
;; (require 'poly-markdown)
;; ;;
;; ;; key config
;; (defun polymode-key-hook ()
;;   (define-key polymode-mode-map (kbd "C-c n") 'polymode-next-chunk-same-type)
;;   (define-key polymode-mode-map (kbd "C-c p") 'polymode-previous-chunk-same-type)
;;   )
;; (add-hook 'polymode-key-hook 'poly-markdown+r-hook)	; undefined
;; (add-hook 'polymode-key-hook 'polymode-select-mode-hook)	; not working
;; (add-hook 'polymode-key-hook 'ess-mode-hook)	; not working


;; Markdown mode
;; http://jblevins.org/projects/markdown-mode/
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


;;; Graphviz mode
;; http://ppareit.github.io/graphviz-dot-mode/
(require 'graphviz-dot-mode)
;; Font locking is automatic, indentation uses the same commands as
;; other modes, tab, M-j and C-M-q.  Insertion of comments uses the
;; same commands as other modes, M-; .  You can compile a file using
;; M-x compile or C-c c, after that M-x next-error will also work.
;; There is support for viewing an generated image with C-c p.



;;; Auto-saving buffers
;; auto-save-buffers: Never lose your data
;; http://0xcc.net/misc/auto-save/
;; http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 1 t 'auto-save-buffers)	; auto-save if idle for 1 sec
;;
;; auto-save-buffers-enhanced.el
;; http://qiita.com/ongaeshi/items/8cbd8d3c792476c59a11
;; http://blog.sanojimaru.com/post/20090254216/emacs
(require 'auto-save-buffers-enhanced)
;; Only in Git, CVS, or Subversion directories
;; (auto-save-buffers-enhanced-include-only-checkout-path t)
;; Timing of save
;; (setq auto-save-buffers-enhanced-interval 1)	; 0.5 sec by default
;; Quiet save
(setq auto-save-buffers-enhanced-quiet-save-p t)
;; Activate
(auto-save-buffers-enhanced t)
;; (setq auto-save-buffers-enhanced-include-regexps '(".+"))
;; (setq auto-save-buffers-enhanced-exclude-regexps '("^not-save-file" "\\.ignore$"))



;;; init-loader.el 
;; https://github.com/emacs-jp/init-loader
;; http://d.hatena.ne.jp/hiro_nemu/20140118/1390058851
;; http://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;; http://shibayu36.hatenablog.com/entry/20101229/1293624201
(require 'init-loader)
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

