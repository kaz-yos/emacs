;;
;; ~/.emacs.d/init.el for cocoa emacs 24.3.1 (source with inline patch) on Mac OS X 10.8.4
;; Reference: http://sakito.jp/emacs/emacs24.html#ime
;;

;;
;; Additional load-paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;  For ess installed with DESTDIR=/usr/local	; obsolete now ess installed via package.el
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/ess/")	; 20130428 12.09 reinstalled
;;
;; For python-mode (configured further below)
;; (add-to-list 'load-path "~/.emacs.d/plugins/python-mode.el-6.1.1/") 
;;
;;
;; System-specific configuration
;; http://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp
(when (eq system-type 'darwin)
  ;; Mac only
  ;; $PATH for external commands		; Necessary for AUCtex platex coordination
  ;; http://emacswiki.org/emacs/EmacsApp
  ;; http://rforge.org/2011/08/16/sane-path-variable-in-emacs-on-mac-os-x/
  ;; You can use them to have the same PATH as .bashrc sets
  (if (not (getenv "TERM_PROGRAM"))
      (setenv "PATH"
	      (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
  )
;
;; exec-path-from-shell.el
;; This could be used instead of the code above 
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))


;;
;; Configuration without external .el file dependencies comes first ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


;; default.el in Vincent Goulet's distribution
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/Emacs-23.3-modified-3/default.el
;;
;;; Nice options to have On by default
(mouse-wheel-mode t)				; activate mouse scrolling
(global-font-lock-mode t)			; syntax highlighting
(transient-mark-mode t)				; sane select (mark) mode
(delete-selection-mode t)			; entry deletes marked text
(show-paren-mode t)				; match parentheses
;; http://ergoemacs.org/emacs/emacs_make_modern.html
;; (setq show-paren-style 'expression)		; highlight entire bracket expression (annoying)
(add-hook 'text-mode-hook 'turn-on-auto-fill)	; wrap long lines in text mode
(column-number-mode 1)				; show (row, col) number
;;
;; Smooth Japanese input
;; http://suzukima.hatenablog.com/entry/2012/08/16/232210
(setq show-paren-delay 0.125)			; Compatibility with Japanese


;; Suppress all dialog box completely, even file open dialogue. No need for mouse!
;; http://www.gnu.org/s/libtool/manual/emacs/Dialog-Boxes.html
(setq use-dialog-box nil)


;; Use Mac OS X system trash
;; http://www.masteringemacs.org/articles/2010/12/30/making-deleted-files-trash-can/
;; http://www.reddit.com/r/emacs/comments/iuyef/emacs_on_mac/
(when (eq system-type 'darwin)
  ;; Mac-only
  (setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs")
  )


;; Bars: Menu bar & scroll bar only
;; http://www.emacswiki.org/emacs/FullScreen#toc7
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode t)


;; Show the current directory in the frame bar
;; http://stackoverflow.com/questions/8945056/emacs-how-to-show-the-current-directory-in-the-frame-bar
(setq frame-title-format '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))


;; winner-mode		; Obsolete Git does not mess with windows
;; http://www.emacswiki.org/emacs/WinnerMode
;; Default: C-c <left> to undo window rearragement. C-c <right> to redo.
(winner-mode t)
(global-set-key (kbd "M-<left>")	'winner-undo)		; M-<left>  to undo
(global-set-key (kbd "M-<right>")	'winner-redo)		; M-<right> to redo


;; Swap buffers with C-x /
;; http://stackoverflow.com/questions/1510091/with-emacs-how-do-you-swap-the-position-of-2-windows
(defun swap-buffer ()
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))
(global-set-key (kbd "C-x /") 'swap-buffer)		; Enabled for everywhere


;; Window management
;;
;; Useful shortcuts
;; http://stackoverflow.com/questions/2901198/useful-keyboard-shortcuts-and-tips-for-ess-r
;; C-tab to switch to other window.
;; (global-set-key [C-tab] 'other-window)
;;
;; C-tab to split or switch to other window. Book by rubikitch p74
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
;; http://stackoverflow.com/questions/916797/emacs-global-set-key-to-c-tab
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally)) ; When there's only one window, split horizontally.
  (other-window 1))
;; (global-set-key [C-tab] 'other-window-or-split)
;; (global-set-key (kbd "C-TAB") 'other-window-or-split)
;; (global-set-key (kbd "C-<tab>") 'other-window-or-split)
(global-set-key (kbd "<C-tab>") 'other-window-or-split)
;; Reversal
;; http://stackoverflow.com/questions/143072/in-emacs-what-is-the-opposite-function-of-other-window-c-x-o
;; (global-set-key [C-S-tab] 'previous-multiframe-window)	;; Added by K
;; (global-set-key (kbd "C-S-TAB") 'previous-multiframe-window)	;; Added by K
;; (global-set-key (kbd "C-S-<tab>") 'previous-multiframe-window)	;; Added by K
(global-set-key (kbd "<C-S-tab>") 'previous-multiframe-window)	;; Added by K
;;
;; Control and up/down arrow keys to search history with matching what you've already typed:
;; (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)	; Overlap with Auto-scroll
;; (define-key comint-mode-map (kbd "C-<up>") 'comint-previous-matching-input-from-input)	; Overlap with Auto-scroll
;; (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
;;
;; other-frame with C-x o, instead of other-window
(global-set-key (kbd "C-x o") 'other-frame)


;; Unique buffer names
;; rubikitch book p84
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)	; rubikitch
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;; Scroll window with C-t/C-v
;; transpose-char changed to cua-scroll-down
(global-set-key (kbd "C-t") 'cua-scroll-down)	; C-t to scroll down, C-v to scroll up
;;
;; Scroll just one line when hitting bottom of window
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-conservatively 10000)
;;
;; Scroll one line at a time (less "jumpy" than defaults)
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))	; one line at a time
;;(setq mouse-wheel-progressive-speed nil)		; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)			; scroll window under mouse
;;(setq scroll-step 1)					; keyboard scroll one line at a time
;;
;; Scroll other window with M-up/M-down	; Conflict with paredit. Use C-M-(S)-v
;; (global-set-key [M-up]		'scroll-other-window-down)
;; (global-set-key [M-down]	'scroll-other-window)
(global-set-key (kbd "M-<up>")		'scroll-other-window-down)
(global-set-key (kbd "M-<down>")	'scroll-other-window)


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


;; Get file path or URL from current line
;; http://www.gnu.org/s/libtool/manual/emacs/FFAP.html
(ffap-bindings)


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


;; Keyboard modification
;;
;; C-h for delete, C-? for help
;; http://www.emacswiki.org/emacs-en/BackspaceKey
(keyboard-translate ?\C-h ?\C-?)
;; (global-set-key [(control h)] 'delete-backward-char)
;; (global-set-key [(control ?)] 'help-command)
;;
(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Command key as Meta key, Option key untouched
  ;; http://www.emacswiki.org/emacs/MetaKeyProblems#toc15
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  ;;
  ;; Mac Binding modifier keys
  ;; http://www.emacswiki.org/emacs/EmacsForMacOS#toc23
  ;; mac-function-modifier
  ;; mac-control-modifier
  ;; mac-command-modifier
  ;; mac-option-modifier
  ;; mac-right-command-modifier
  ;; mac-right-control-modifier
  ;; mac-right-option-modifier
  ;; values can be 'control, 'alt, 'meta, 'super, 'hyper, nil (setting to nil allows the OS to assign values)
  )


;; Added automatically by emacs
(put 'upcase-region	'disabled nil)
(put 'downcase-region	'disabled nil)


;; Rectangle is not used. 
;; Common User Access mode for column editing: Activated by C-RET while selecting text
;; http://tech.kayac.com/archive/emacs-rectangle.html
;; http://trey-jackson.blogspot.com/2008/10/emacs-tip-26-cua-mode-specifically.html
;; http://stackoverflow.com/questions/3750332/how-do-i-force-a-binding-in-emacs
;; (cua-mode nil)
(cua-mode t)
(setq cua-enable-cua-keys nil)			; C-x C-c C-v left intact
(setq cua-rectangle-mark-key (kbd "<C-S-return>")) ; <C-S-return> for rectangle
;; (setq cua-keep-region-after-copy t)		; Keep selection after copying (Mac/Win-like)


;; No auto filling in text mode
;; http://tomikura.s2.xrea.com/linux/install/emacs.html
(setq fill-column 80)
(setq text-mode-hook '(lambda () (auto-fill-mode 0)))
;;(setq default-major-mode 'text-mode)	; Obsolete as of version 23.2
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html
(setq-default major-mode 'text-mode)


;; Language settings
;;
;; Unicode use
;; http://d.hatena.ne.jp/syou6162/20080519/1211133695
(set-locale-environment "utf-8")
(setenv "LANG" "en_US.UTF-8")
;; (setenv "LANG" "ja_JP.UTF-8")
;; http://www.emacswiki.org/emacs/EmacsForMacOS#toc18
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;
(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Japanese font setting that works
  ;; http://sakito.jp/emacs/emacs23.html#id17
  ;; Method 1 for Japanese fonts ; This works with ESS(R). Fixed width ok with rescaling. Greek goes bad.
  ;; Method 2 for Japanese fonts ; This works with ESS(R). Fixed width breaks with rescaling.
  ;;
  ;; Cocoa Emacs Font Settings ; Best one so far. Good rescaling, and working with Greek letters φ phi
  ;; http://d.hatena.ne.jp/setoryohei/20110117
  ;;
  ;; Method 1			; This works with ESS(R). Fixed width ok with rescaling. Greek φ(phi) works.
  ;; default-frame font configured
  ;; New font set made, it is then chosen as default-frame-alist font.
  ;; Fontset made with crease-fontse-from-ascii-font
  ;; Font selected by family name, font-spec object made.
  ;;
  ;; Fontset made
  (let* ((fontset-name "myfonts")				; Fontset name
	 (size 14)					; Font size one of [9/10/12/14/15/17/19/20/...]
	 (asciifont "Menlo")				; ascii font
	 (jpfont "Hiragino Maru Gothic ProN")		; Japanese font
	 (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
	 (fontspec (font-spec :family asciifont))
	 (jp-fontspec (font-spec :family jpfont))
	 (fsn (create-fontset-from-ascii-font font nil fontset-name)))
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)	; Half-sized katakana
    (set-fontset-font fsn '(#x0080 . #x024F) fontspec)	; Latin with pronounciation marks (as in German)
    (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)	; Greek
    )
  ;; Fontset for default-frame
  (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))
  ;; Relative size of different fonts
  (dolist (elt '(("^-apple-hiragino.*" . 1.2)
		 (".*osaka-bold.*" . 1.2)
		 (".*osaka-medium.*" . 1.2)
		 (".*courier-bold-.*-mac-roman" . 1.0)
		 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
		 (".*monaco-bold-.*-mac-roman" . 0.9)))
    (add-to-list 'face-font-rescale-alist elt))
  ;; Fontset configured to default face. Fixes default-frame-alist being ignored at startup.
  (set-face-font 'default "fontset-myfonts")
  ;;
  ;; ;; Method 2	;; Not tested yet. Same configuration with a different method.
  ;; ;; Font set for frame
  ;; (let* ((size 14)
  ;;        (asciifont "Menlo")
  ;;        (jpfont "Hiragino Maru Gothic ProN")
  ;;        (h (* size 10))
  ;;        (fontspec (font-spec :family asciifont))
  ;;        (jp-fontspec (font-spec :family jpfont)))
  ;;   (set-face-attribute 'default nil :family asciifont :height h)
  ;;   (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  ;;   (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  ;;   (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  ;;   (set-fontset-font nil '(#x0080 . #x024F) fontspec)
  ;;   (set-fontset-font nil '(#x0370 . #x03FF) fontspec)
  ;;   )
  ;; ;;
  ;; (dolist (elt '(("^-apple-hiragino.*" . 1.2)
  ;; 	       (".*osaka-bold.*" . 1.2)
  ;; 	       (".*osaka-medium.*" . 1.2)
  ;; 	       (".*courier-bold-.*-mac-roman" . 1.0)
  ;; 	       (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
  ;; 	       (".*monaco-bold-.*-mac-roman" . 0.9)))
  ;;   (add-to-list 'face-font-rescale-alist elt))      
  )


;; Use emacsclient
;; .profile: export EDITOR="emacsclient"
;; http://www.emacswiki.org/emacs/EmacsClient
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))
;; (server-start)		; Start server
;; (defvar server-buffer-clients)
;; (when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
;;   (server-start)
;;   (defun fp-kill-server-with-buffer-routine ()
;;     (and server-buffer-clients (server-done)))
;;   (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))



;; ;; Shell script send		; Deactivated in favor of essh.el
;; ;; http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
;; (defun sh-send-line-or-region (&optional step)
;;   (interactive ())
;;   (let ((proc (get-process "shell"))
;;         pbuf min max command)
;;     (unless proc
;;       (let ((currbuff (current-buffer)))
;;         (shell)
;;         (switch-to-buffer currbuff)
;;         (setq proc (get-process "shell"))
;;         ))
;;     (setq pbuff (process-buffer proc))
;;     (if (use-region-p)
;;         (setq min (region-beginning)
;;               max (region-end))
;;       (setq min (point-at-bol)
;;             max (point-at-eol)))
;;     (setq command (concat (buffer-substring min max) "\n"))
;;     (with-current-buffer pbuff
;;       (goto-char (process-mark proc))
;;       (insert command)
;;       (move-marker (process-mark proc) (point))
;;       ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
;;     (process-send-string  proc command)
;;     (display-buffer (process-buffer proc) t)
;;     (when step 
;;       (goto-char max)
;;       (next-line))
;;     ))
;; ;;
;; (defun sh-send-line-or-region-and-step ()
;;   (interactive)
;;   (sh-send-line-or-region t))
;; (defun sh-switch-to-process-buffer ()
;;   (interactive)
;;   (pop-to-buffer (process-buffer (get-process "shell")) t))
;; ;;
;; ;; Below modified from original
;; (require 'sh-script)	; sh-mode-map is defined by sh-script.el included in default install
;; (define-key sh-mode-map "\C-j"      'sh-send-line-or-region-and-step)
;; (define-key sh-mode-map "\C-c \C-z" 'sh-switch-to-process-buffer)	
;; ;; Scrolling is defined in ESS configuration
;; ;; (setq comint-scroll-to-bottom-on-output t) 
;; ;;



;; Automatically close brackets and parentheses
;; http://www.emacswiki.org/emacs/ESSAutoParens
;; http://www.emacswiki.org/emacs/SkeletonPair
;; 20130426 enable skeleton-pair in iESS mode only.
;; Work around for problem with inserting before a parenthesis.
;; In other mode the default electric-pair-mode works good
;;
;; Define a function for local activation
;; (defun my-skeleton-pair-enable ()
;;   (setq skeleton-pair t)		; on 20130426
;;   (setq skeleton-pair-on-word t)	; on 20130426
;;   (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;;   (local-set-key (kbd "[") 'skeleton-pair-insert-maybe) ; NOT Better handled with smartchr.el
;;   (local-set-key (kbd "\{") 'skeleton-pair-insert-maybe) ; This does not work? added \ 20130426
;;   (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;;   (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)	; Not useful for R, or lisp
;;   (local-set-key (kbd "\`") 'skeleton-pair-insert-maybe)	; Not useful for R
;;   ;; (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)	; Not useful for R
;;   )
;; 
;; (add-hook 'ess-mode-hook 'my-skeleton-pair-enable)
;; (add-hook 'inferior-ess-mode-hook 'my-skeleton-pair-enable)
;; Better to use them everywhere along with electric pair?? Started trying on 20130426
  (setq skeleton-pair t)		; on 20130426
  (setq skeleton-pair-on-word t)	; on 20130426
  (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "[") 'skeleton-pair-insert-maybe) ; NOT Better handled with smartchr.el
  (global-set-key (kbd "\{") 'skeleton-pair-insert-maybe) ; This does not work?  added \ 20130426
  (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)	; Not useful for lisp
  ;; (global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)	; Not useful for R
  ;; (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)	; Not useful for R
;;
;;
;; electric-pair-mode to automatically close brackets (New in emacs 24.1)
;; http://www.emacswiki.org/emacs/AutoPairs
;; These alone cannot place () around words? Use with skeleton-pair 20130426
(electric-pair-mode t)			; 
(setq electric-pair-skip-self nil)	; Do not prevent overlapping ] before ]


;; ;; External dependencies starting here. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; el-get.el package system 2013-02-26
;; https://github.com/dimitri/el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")	; This is configured at the top.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(el-get 'sync)


;; MELPA
;; http://melpa.milkbox.net/#installing
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Marmalade
;; http://qiita.com/items/e81fca7a9797fe203e9f
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
; Initialize
(package-initialize)
; melpa.el
(require 'melpa)


;; Mac-only configuration
(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Change to English in minibuffer (require inline patch. No .el dependency)
  ;; http://molekun.blogspot.com/2011/03/homebrewemacs233.html
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)

  ;; Spell checker (require aspell. No .el dependency)
  ;; http://blog.bungu-do.jp/archives/2426	;; sudo port install aspell aspell-dict-en
  (setq ispell-program-name "/usr/local/bin/aspell")

  ;; Mac AppleScrip compatibility
  ;; http://d.hatena.ne.jp/tequilasunset/touch/20110104/p2
  ;; https://gist.github.com/764745#file_my_mac_key_mode.el
  (defun do-applescript+ (&rest scripts)
    "Like `do-applescript', but execute concatenated SCRIPTS.
In case the execution fails, return an error."
    (condition-case err
	(do-applescript
	 (apply 'concat scripts))
      (error err)))
  ;;
  (defun show-in-finder (&optional path behind)
    "Display current file/directory in a Finder window"
    (interactive)
    (let ((item (or path
		    buffer-file-name
		    (and (eq major-mode 'dired-mode) default-directory))))
      (cond
       ((not (stringp item)))
       ((file-remote-p item)
	(error "This item is located on a remote system."))
       (t
	(setq item (expand-file-name item))
	(do-applescript+
	 "tell application \"Finder\" to select (\""
	 item
	 "\" as POSIX file)\n"
	 (unless behind
	   "tell application \"Finder\" to activate"))))))
  ;;
  (defun open-terminal (&optional path behind)
    "Launch Terminal and go to the relevant directory"
    (interactive)
    (let ((item (or path default-directory)))
      (cond
       ((not (stringp item)))
       ((file-remote-p item)
	(error "This item is located on a remote system."))
       ((file-directory-p item)
	(setq item (expand-file-name item))
	(do-applescript+
	 "tell application \"Terminal\" to do script "
	 "with command \"cd \" & quoted form of \""
	 item
	 "\"\n"
	 (unless behind
	   "tell application \"Terminal\" to activate")))
       (t
	(error "An error occured")))))


  ;; cmigemo (installed from brew)
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


;; M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 ;; '(cua-mode t nil (cua-base))	; turn off cua-mode
 '(custom-enabled-themes nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; minor-mode-hack.el
;; M-x show-minor-mode-map-priority to see minor mode priority
(require 'minor-mode-hack)


;; bm.el	Within-file bookmarking
;; See ~/.emacs.d/elpa/bm-readme.txt
;; http://d.hatena.ne.jp/peccu/20100402
;; Saving bookmarks
(setq-default bm-buffer-persistence t)
(setq bm-repository-file "~/.emacs.d/bm-el-repository")
(setq bm-restore-repository-on-load t)	; bm-readme.txt
;; Load
(require 'bm)
;; Load on startup
(add-hook 'after-init-hook		'bm-repository-load)
;; Restore when finding file
(add-hook 'find-file-hooks		'bm-buffer-restore)
;;
;; Saving on killing and saving a buffer
(add-hook 'kill-buffer-hook		'bm-buffer-save)
(add-hook 'auto-save-hook		'bm-buffer-save)
(add-hook 'after-save-hook		'bm-buffer-save)
;; Version control (Rubikitch book p116)
(add-hook 'after-revert-hook		'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook	'bm-buffer-save)
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))
;; Keyboard
(global-set-key (kbd "M-SPC")	'bm-toggle)	; Conflict with IM. Use ESC-SPC, which is the same
(global-set-key (kbd "M-]")	'bm-next)
(global-set-key (kbd "M-[")	'bm-previous)


;; Templates
;;
;; autoinsert.el
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/autoinsert/")
(define-auto-insert "\\.Rmd$" "knitr.Rmd")
(define-auto-insert "\\.sas$" "SAS.sas")


;; Auto-completion
;;
;; auto-complete.el, auto-complete-config.el, fuzzy.el, popup.el downloaded from below URL
;; https://github.com/auto-complete/auto-complete
;; http://cx4a.org/software/auto-complete/manual.html
;; (require 'popup)				; https://github.com/auto-complete/popup-el (called auto)
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
;;
;; Auto-complete for ESS configuration
;; http://www.emacswiki.org/emacs/ESSAuto-complete 
(setq 
 ;; ac-auto-show-menu 1
 ;; ac-candidate-limit nil
 ac-delay 0			; Faster than default 0.1
 ;; http://stackoverflow.com/questions/17309773/emacs-autocomplete-inside-python-string
 ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
 ac-disable-faces nil		; auto-complete everywhere, even within quotes, comments
 ;; ac-ignore-case 'smart		; Treat them smartly
 ac-ignore-case nil		; Treat them strictly
 ac-menu-height 20			; 20 candidates
 ac-use-quick-help nil		; No pop up help!
 ;; ac-quick-help-delay 1.5
 ;; ac-quick-help-prefer-pos-tip t
 )
;; Less anoying settings
;; http://cx4a.org/software/auto-complete/manual.html#Not_to_complete_automatically
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;;
;; http://www.emacswiki.org/emacs/ESSAuto-complete
;; (define-key ac-completing-map [tab] 'ac-complete)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
;; (define-key ac-completing-map [tab] nil)
;; (define-key ac-completing-map [return] 'ac-complete)	; configured again at end
(define-key ac-completing-map (kbd "RET") 'ac-complete) ; configured again at end
;;
;; Trigger key
;; http://cx4a.org/software/auto-complete/manual.html#Trigger_Key
(ac-set-trigger-key "TAB")
;; (ac-set-trigger-key (kbd "TAB")) ; This does not work


;; YAsnippet ; Automatic template inserting system.
;;
;; https://github.com/capitaomorte/yasnippet
;; $ git clone http://github.com/capitaomorte/yasnippet.git in plugins folder
;; M-x install-elisp-from-emacswiki RET yasnippet-config.el RET
(require 'yasnippet)		; not yasnippet-bundle
;; (require 'yasnippet-config)	; Book by rubikitch p127 ; no need for elpa 2013-02-24
;; (yas/initialize)		; No need now?
;; (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")
(require 'auto-complete-yasnippet)
;;
;; http://fukuyama.co/yasnippet	; turned off after elpa installation 2013-02-24
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/plugins/snippets"		; Folder for my snippets
;;         "~/.emacs.d/plugins/yasnippet/snippets" ; Folder for default snippets
;;         ))
;;
(yas-global-mode 1)
;; Key-bind for expanding
;; Insert default snippet
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; Open a buffer to create a new snippet
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; View/edit snippets
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


;;; Emacs Speaks Statistics (ESS) for emacs
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
;; Now installed via elpa
;; 
;; Note on version 12.09 (source install)
;; From version 12.09 on, these files have been replaced with ess-noweb-*.
;; These files must be deleted manually because make install do not replace them.
;; Jun 17	2012 noweb-font-lock-mode.elc
;; Jun 17	2012 noweb-mode.elc
;; Jun  7	2012 noweb-font-lock-mode.el
;; Jun  7	2012 noweb-mode.el
;;
;; Some condigurations were taken from vgoulet Emacs distribution configuration files
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/
(require 'ess-site)
(require 'ess-eldoc)				; Slows cursor movements slightly?
;; (setq ess-eldoc-show-on-symbol t)		; Shows eldoc when cursor is on function name (Causes errors)
;;
;; Installed via el-get. add functionalities to ESS.
(require 'ess-edit)
;;
;; No history, no saving!
(setq-default inferior-R-args "--no-restore-history --no-save ")
;;
;; Set code indentation following the standard in R sources.
;; http://ess.r-project.org/Manual/ess.html#Indenting
;; https://svn.r-project.org/ESS/trunk/lisp/ess-custom.el
;; ESS provides: DEFAULT, OWN, GNU, BSD, K&R, C++, RRR, CLB.
;;                                 DEF GNU BSD K&R C++ RRR CLB
;; ess-indent-level                  2   2   8   5   4   4   2
;; ess-continued-statement-offset    2   2   8   5   4   4   4
;; ess-brace-offset                  0   0  -8  -5  -4   0   0
;; ess-arg-function-offset           2   4   0   0   0   4   0
;; ess-expression-offset             4   2   8   5   4   4   4
;; ess-else-offset                   0   0   0   0   0   0   0
;; ess-close-brace-offset            0   0   0   0   0   0   2
(setq ess-default-style 'RRR)	; Common R chosen
;;
;; Reflect file changes occured outside emacs (For SAS and RStudio)
(add-hook 'ess-mode-hook 'turn-on-auto-revert-mode)
;;
;; Key assignment for delete trailing whitespace			; M-p to nuke trailing whitespace
(add-hook 'ess-mode-hook (setq ess-nuke-trailing-whitespace-p t))
(define-key ess-mode-map (kbd "M-p") 'ess-nuke-trailing-whitespace)
;;
;; Underscore preservation in ESS
;; http://www.r-bloggers.com/a-small-customization-of-ess/
(setq ess-S-assign-key (kbd "C-="))	; C-= gives <-
(ess-toggle-S-assign-key t)		; enable above key definition
(ess-toggle-underscore nil)		; leave my underscore key alone!
;;
;; Smart TAB completion in R scripts, similar to iESS behavior.	; Since 12.04
;; (setq ess-tab-complete-in-script t)
(setq ess-tab-complete-in-script nil)	; Trying out nil 2013-03-03
(setq ess-first-tab-never-complete nil)	; Trying out nil 2013-03-01
;;
;; Must-haves for ESS
;; http://www.emacswiki.org/emacs/CategoryESS
;; (setq ess-eval-visibly-p nil)		; faster if off (deprecated)
(setq ess-eval-visibly nil)		; New in 12.09-1
(setq ess-ask-for-ess-directory nil)	; Don't ask for directory
;;
;; Auto-scrolling of R console to bottom and Shift key extension
;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/ESSShiftEnter
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;;
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w1 "*R*")	; R on the left (w1)
        (set-window-buffer w2 w1name)	; script on the right (w2)
	;; (set-window-buffer w2 "*R*")
	;; (set-window-buffer w1 w1name)
	;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Selecting-Windows.html
	(select-window w2)		; Select script (w2) Added
	)))
;;
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
;;
;; Function to toggle $ in syntax table 2013-08-06
(defun toggle-dollar ()
  "Toggle status of $ in the syntax table"
  (interactive)
  (if (equal " " (char-to-string (char-syntax ?$)))
      (modify-syntax-entry ?$  "_"  S-syntax-table)
      ;; (modify-syntax-entry ?$ "_")
    (modify-syntax-entry ?$  " "  S-syntax-table)
    ;; (modify-syntax-entry ?$ " ")
    ))
;;
(add-hook 'ess-mode-hook		; For ESS mode
          '(lambda()
	     ;; (local-unset-key [C-tab] 'ess-sas-backward-delete-tab)	; Does not work with the one below??
	     ;; (local-set-key [(control return)] 'my-ess-eval)
             ;; (local-set-key [(shift return)] 'my-ess-eval)
	     (local-set-key (kbd "<S-return>") 'my-ess-eval)
	     ;; (local-unset-key (kbd "<C-return>"))		; Unset cua. Does not work
	     (local-set-key (kbd "<C-return>") 'my-ess-eval)	; Change to my-ess-eval
	     (cua-mode -1)					; disable CUA
	     (local-set-key (kbd "C-c 4") 'toggle-dollar)	; Toggle $ in S-syntax-table
	     (modify-syntax-entry ?$  " "  S-syntax-table)	; $ as whitespace in S
	     ;; (cua-mode)					; does not work
	     ))
;; (add-hook 'ess-mode-hook (modify-syntax-entry ?$  "."  S-syntax-table)) ; does not work either
(add-hook 'inferior-ess-mode-hook	; For iESS mode
          '(lambda()
	     (local-set-key (kbd "C-c w") 'ess-execute-screen-options)
             ;; (local-set-key [C-up] 'comint-previous-input)
             ;; (local-set-key [C-down] 'comint-next-input)
             (local-set-key (kbd "C-<up>") 'comint-previous-input)
             (local-set-key (kbd "C-<down>") 'comint-next-input)
	     ))
(add-hook 'Rnw-mode-hook		; For Rnw mode
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)
             ;; (local-set-key (kbd "S-RET") 'my-ess-eval)
	     ))
;;
;; ess-trace-bug.el		; filtering ++++ > ??? Not working
;; http://code.google.com/p/ess-tracebug/
(require 'ess-tracebug)
(setq ess-use-tracebug t)	; permanent activation
;;
;; *.Rmd files invoke r-mode	; Temporary fix for R markdown files
(setq auto-mode-alist
      (cons '("\\.Rmd$" . r-mode) auto-mode-alist))
;;
;; Tooltip included in ESS
;; (define-key ess-mode-map "\C-c\C-g" 'ess-describe-object-at-point)	; Changed from C-c C-d C-e
(setq ess-describe-at-point-method 'tooltip)		; 'tooltip or nil (buffer)
;;
;; ess-R-object-popup.el
;; https://github.com/myuhe/ess-R-object-popup.el
(require 'ess-R-object-popup)
;; (define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)
(define-key ess-mode-map (kbd "C-c C-g") 'ess-R-object-popup)
;; Configuration for different objects
(setq ess-R-object-popup-alist
      '((numeric    . "summary")
        (factor     . "table")
        (integer    . "summary")
        (lm         . "summary")	
	(glm        . "summary")			; added
	(gls        . "summary")			; added
	(lme        . "summary")			; added
	(glht	    . "summary")			; added	
	(survfit    . "survival:::print.survfit")	; added
	(survdiff   . "survival:::print.survdiff")	; added	
	(coxph	    . "survival:::print.coxph")		; added
        (other      . "str")))
;;
;;
;; ess-R-data-view.el
;; https://github.com/myuhe/ess-R-data-view.el/blob/master/README.org
;; (define-key ess-mode-map "\C-c\C-d\C-e" 'ess-R-dv-pprint)
(define-key ess-mode-map (kbd "C-c C-d C-e") 'ess-R-dv-pprint)
;;
;; ESS julia language
;; https://github.com/emacs-ess/ESS/wiki/Julia
;; excecutable file
(setq inferior-julia-program-name "/Applications/Julia.app/Contents/Resources/julia/bin/julia-release-basic")
;;
;; ESS SAS configuration
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
;; (add-hook 'ess-mode-hook	; For SAS mode
;;           '(lambda()
;;              (local-unset-key [C-tab] 'ess-sas-backward-delete-tab)))	; Unset C-tab
;;
;; Regard dollar signs as a white space to change syntax highlighting with highlight-symbol.el
;; http://www.emacswiki.org/emacs/EmacsSyntaxTable
;; http://stackoverflow.com/questions/1771102/changing-emacs-forward-word-behaviour
;; (modify-syntax-entry ?$ " " ess-mode-syntax-table)	; This is overriden. does not work.
;; (modify-syntax-entry ?$  " "  S-syntax-table)	; Overriend by ESS upon loading
;;
;; edited ess-s-l.el directly
;; (modify-syntax-entry ?$  " "  S-syntax-table); foo$comp = 1 symbol(completion)
;; ;; (modify-syntax-entry ?$  "_"  S-syntax-table); foo$comp = 1 symbol(completion)



;; essh.el for shell (like ESS for R
;; http://www.emacswiki.org/emacs/essh.el
(require 'essh)
(defun essh-sh-hook ()                                             
  (define-key sh-mode-map (kbd "C-c C-r") 'pipe-region-to-shell)        
  (define-key sh-mode-map (kbd "C-c C-b") 'pipe-buffer-to-shell)        
  (define-key sh-mode-map (kbd "C-c C-j") 'pipe-line-to-shell)          
  (define-key sh-mode-map (kbd "C-c C-n") 'pipe-line-to-shell-and-step) 
  (define-key sh-mode-map (kbd "C-c C-f") 'pipe-function-to-shell)      
  (define-key sh-mode-map (kbd "C-c C-d") 'shell-cd-current-directory)) 
(add-hook 'sh-mode-hook 'essh-sh-hook)                             
;;
;; Changed from ESS
;; Auto-scrolling of R console to bottom and Shift key extension
;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/ESSShiftEnter
(defun my-essh-start-shell ()
  (interactive)
  (if (not (member "*shell*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (shell)					; activate shell
        (set-window-buffer w1 "*shell*")	; shell on the left
        (set-window-buffer w2 w1name)
	(select-window w2))))			; select script
;;
(defun my-essh-eval ()
  (interactive)
  (my-essh-start-shell)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'pipe-region-to-shell)
    (call-interactively 'pipe-line-to-shell-and-step)))
;;
(add-hook 'sh-mode-hook		; For shell script mode
          '(lambda()
             (local-set-key (kbd "S-<return>") 'my-essh-eval)))



;; TeX environments
;;
;; AUCtex
;; http://wikemacs.org/wiki/AUCTeX
;; http://www.gnu.org/software/auctex/manual/auctex.html
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;;
;; Japanese setting
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX
(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-LaTeX-default-style "jsarticle")
;;
;; Engines
(setq TeX-engine-alist '((ptex "pTeX" "eptex" "platex" "eptex")
                         (uptex "upTeX" "euptex" "uplatex" "euptex")))
(setq TeX-engine 'ptex)
;;
;; TeX-PDF mode (no DVI intermediate file)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;;
;; Interactive mode for errors
(add-hook 'LaTeX-mode-hook 'TeX-interactive-mode)
;;
;;
;; (See https://github.com/vitoshka/ac-math#readme for more)
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources))
  )
;;
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
;;
;; If you are using 'flyspell' you might want to activate the
;; [workaround](http://www.emacswiki.org/emacs/AutoComplete#toc6):
(ac-flyspell-workaround)


;; Markdown mode
;; http://jblevins.org/projects/markdown-mode/
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; Buffer Management
;;
;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)
;;
;; ;; buffer-menu+.el		; Does not work with 24.3
;; ;; http://www.emacswiki.org/emacs/BufferMenuPlus#BufferMenu
;; (require 'buff-menu+)
;; (add-to-list 'same-window-buffer-names "*Buffer List*")		;; Open in current window
;; (global-set-key (kbd "C-x M-b") 'buffer-menu-other-window)	;; Open in other window
;;
;; Sort by mode name
;; Number corresponds to buffer-menu+ column number
;; (setq Buffer-menu-sort-column 5) ;; 1CRM, 2Buffer, 3Size, 4Time, 5Mode, 6File
;;
;; bs-show for buffer listing. Use c to make it more complete
;; http://www.emacswiki.org/emacs/BufferSelection
;; (global-set-key (kbd "C-x b") 'bs-show)
;;
;; iswitchb.el Strengthen swtich-to-buffer ; not useful
;; http://www.emacswiki.org/emacs/IswitchBuffers
;; (iswitchb-mode 1)
;; (setq read-buffer-function 'iswitchb-read-buffer)
;; (setq iswitchb-regexp nil)
;; (setq iswitchb-prompt-newbuffer nil)


;; tempbuf.el	Auto-delete unused idle buffers such as dired
;; http://www.emacswiki.org/emacs/tempbuf.el
(require 'tempbuf)
(add-hook 'help-mode-hook		'turn-on-tempbuf-mode)	; Idle help closed
(add-hook 'find-file-hooks		'turn-on-tempbuf-mode)	; Idle unedited files closed
(add-hook 'dired-mode-hook		'turn-on-tempbuf-mode)	; Idle dired closed
(add-hook 'ess-help-mode-hook		'turn-on-tempbuf-mode)	; Idle ESS help closed
(add-hook 'completion-list-mode-hook	'turn-on-tempbuf-mode)	; Idle completion closed
(add-hook 'vc-annotate-mode-hook	'turn-on-tempbuf-mode)	; Idle VC annotate closed
(add-hook 'log-view-mode-hook		'turn-on-tempbuf-mode)	; Idle VC change log closed
(add-hook 'diff-mode-hook		'turn-on-tempbuf-mode)	; Idle VC diff closed
(add-hook 'Snippet-mode-hook		'turn-on-tempbuf-mode)  ; Idle Snippets closed
;; (add-hook 'fundamental-mode-hook 'turn-on-tempbuf-mode)	; Idle auto-install closed. Not working


;; Recent files extended
;; Book by rubikitch p87
;; http://d.hatena.ne.jp/rubikitch/20091224/recentf
;; http://www.mygooglest.com/fni/dot-emacs.html
(setq recentf-save-file  "~/.emacs.d/recentf")		; save file within ~/.emacs.d
(setq recentf-max-saved-items 3000)
(require 'recentf-ext)
(global-set-key (kbd "C-S-z") 'recentf-open-files)	; helm has priority now
;; (global-set-key (kbd "C-z") 'helm-recentf)		; helm version. activated at helm configuration
;;
;; recentf auto-save
;; http://d.hatena.ne.jp/tomoya/20110217/1297928222
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '("recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))


;; Auto-saving buffers
;; auto-save-buffers: Never lose your data
;; http://0xcc.net/misc/auto-save/
;; http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el
(require 'auto-save-buffers)
(run-with-idle-timer 1 t 'auto-save-buffers)	; auto-save if idle for 1 sec


;; Auto-intall	; This is causing problem !!! 2013-03-04
;; http://www.emacswiki.org/AutoInstall
;; http://www.emacswiki.org/emacs/auto-install.el
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
;;
;; emacs customization book by rubikitch
;; EmacsWiki added for auto-completion list at startup
;; http://spiri-tua-lism.com/?p=451
(when (eq system-type 'darwin)
  ;; Mac-only
  (setq auto-install-use-wget t)
  )
;; This has to be done??
;; sudo ln -s `which wget` /usr/bin/wget
;; ;;
;; THIS FREEZES EMACS IF EMACSWIKI IS DONW!!!!!! Defined at the end of this file.
;; (auto-install-update-emacswiki-package-name t)
;; INSTALL-elisp.el compatilibity
(auto-install-compatibility-setup)
;; ediff associated buffers in one frame
(setq ediff-windows-setup-function 'ediff-setup-windows-plain)


;; Helm (Anything successor)
;; http://sleepboy-zzz.blogspot.com/2012/09/anythinghelm.html	; in general
;; http://sleepboy-zzz.blogspot.com/2013/02/helm-migemo.html	; helm-migemo
;; http://d.hatena.ne.jp/syohex/20121207/1354885367		; some useful tips
(require 'helm-config)
(require 'helm-command)
(require 'helm-descbinds)
;;
(setq helm-idle-delay             0.3
      helm-input-idle-delay       0.3
      helm-candidate-number-limit 200
      helm-M-x-always-save-history t)
;;
(let ((key-and-func
       `(;(,(kbd "C-r")   helm-for-files)	; Not sure if backquote is right
	 (,(kbd "C-z")   helm-for-files)	; Like recentf
         (,(kbd "C-^")   helm-c-apropos)
         (,(kbd "C-;")   helm-resume)
         (,(kbd "M-s")   helm-occur)
         (,(kbd "M-x")   helm-M-x)
         (,(kbd "M-y")   helm-show-kill-ring)
         (,(kbd "M-z")   helm-do-grep)
         (,(kbd "C-S-h") helm-descbinds)
	 )))
  (loop for (key func) in key-and-func
        do (global-set-key key func)))
;;
;; Optional helm packages
(require 'helm-R)
(when (eq system-type 'darwin)
    ;; Mac-only
    ;; http://sleepboy-zzz.blogspot.com/2012/09/anythinghelm.html
    (require 'helm-migemo)	
  )
;;


;; Key-Chord
;;
;; http://www.emacswiki.org/emacs/KeyChord
;; http://d.hatena.ne.jp/rubikitch/touch/20081104/1225745862
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
;;
;; Viper mode activation/deactivation
;; (key-chord-define-global "jk" 'toggle-viper-mode)		; view-mode used now
;; (key-chord-define-global "kl" 'toggle-viper-mode)
;;
;; R object name completion in ESS
(key-chord-define-global "df" 'ess-complete-object-name)		; C-c TAB emulation
(key-chord-define-global "sd" 'comint-dynamic-complete-filename)	; M-TAB emulation
;;
;; auto-complete.el
(key-chord-define-global "ac" 'auto-complete-mode)	; Toggling auto-complete-mode
;;
;; eval-region
;; (key-chord-define-global "er" 'eval-region)		; Evaluate region for elisp. Not useful.
;;
;; count-words
(key-chord-define-global "wc" 'count-words)		; Choose region and activate for word count
;;
;; Comment or Uncomment region
(key-chord-define-global "cr" 'comment-region)
;; (key-chord-define-global "ur" 'uncomment-region)	; Dangerous if ur only
;;
;; replace-string
(key-chord-define-global "rs" 'replace-string)
;;
;; view-mode
(key-chord-define-global "jk" 'view-mode)		; Enter read-only mode
;;
;; crosshairs.el
;; (key-chord-define-global "ch" 'crosshairs-mode)		; Activate continuous crosshairing
;;
;; paredit.el
(key-chord-define-global "pm" 'paredit-mode)		; Toggle paredit-mode
;;
;; flyspell-mode
(key-chord-define-global "fs" 'flyspell-mode)		; Toggle flyspell-mode
;;
;; Align regexp
(key-chord-define-global "nr" 'align-regexp)		; M-x align-regexp.
;;
;; Show in finder
(key-chord-define-global "zx" 'show-in-finder)		; M-x show-in-finder
;;
;; bm.el for bookmarking by highlighting
;; (key-chord-define-global "bm" 'bm-toggle)		; ESC-SPC is good enough
;;
;; show-paren-mode
;; (key-chord-define-global "pm" 'show-paren-mode)		; (show-paren-mode)
;;
;; highlight-sexp-mode.el
(key-chord-define-global "sx" 'highlight-sexp-mode)		; (highlight-sexp-mode)


;; One-key
;;
;; http://d.hatena.ne.jp/rubikitch/20090127/onekey
;; http://d.hatena.ne.jp/tomoya/20090415/1239809615
;; http://www.emacswiki.org/emacs/OneKey
;; Make sure that you also have Lisp:hexrgb.el in your load-path
(when (eq system-type 'darwin)
  ;; Mac-only (error on Win system)
  ;; (require 'one-key)					; one-key.el
  (require 'one-key-default)				; one-key.el loaded together. need to come last
  (require 'one-key-config)				; many templates. active on 20130508
  (one-key-default-setup-keys)				; Enable one-key- menu
  ;; (define-key global-map "\C-x" 'one-key-menu-C-x)	; C-x assigned. breaks other C-x
  )


;; view-mode			; C-x C-r to open in view-mode. Requires viewer.el
;; Book by rubikitch p214-
;; http://d.hatena.ne.jp/rubikitch/20081104/1225745862
;; http://d.hatena.ne.jp/syohex/20110114/1294958917
(setq view-read-only t)
(require 'viewer)
(viewer-stay-in-setup)
(setq viewer-modeline-color-unwritable "tomato"
      viewer-modeline-color-view "orange")
(viewer-change-modeline-color-setup)
(require 'view)
;; less like
(define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
(define-key view-mode-map (kbd "?") 'View-search-regexp-backward )
(define-key view-mode-map (kbd "G") 'View-goto-line-last)
(define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
(define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
;; vi/w3m like
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "j") 'next-line)
(define-key view-mode-map (kbd "k") 'previous-line)
(define-key view-mode-map (kbd "l") 'forward-char)
(define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
(define-key view-mode-map (kbd "K") 'View-scroll-line-backward)


;; crosshairs.el: highlight current line/column using hl-line(+).el/col-highlight.el
;; http://www.emacswiki.org/emacs/CrosshairHighlighting
(require 'crosshairs)
;; (toggle-crosshairs-when-idle t) ; No need for crosshairs when idle
;; (col-highlight-set-interval 60)
;;
;; hl-line+.el: highlight current line only (no column)
;; http://www.emacswiki.org/emacs/HighlightCurrentLine#toc3
;; (require 'hl-line+)		; required by crosshairs already
;; (toggle-hl-line-when-idle t)	; turned on line highlight when idle
;; (toggle-hl-line-when-idle nil)	; turned off line highlight when idle
(hl-line-when-idle-interval 60)
;;
;; To customize the background color
(setq my-highlight-color "light goldenrod yellow")
(set-face-background 'hl-line	    my-highlight-color)   ; Line color
(set-face-background 'col-highlight my-highlight-color)	; Column color
;; (set-face-background 'hl-line	    "light goldenrod yellow")   ; Line color
;; (set-face-background 'col-highlight "light goldenrod yellow")	; Column color
;; (set-face-background 'hl-line "lemon chiffon")		; Line color
;; (set-face-background 'col-highlight "lemon chiffon")		; Column color


;; highligh-symbol for highlighting multiple occurences
;; http://nschum.de/src/emacs/highlight-symbol/
;; http://stackoverflow.com/questions/385661/emacs-highlight-all-occurences-of-a-word
(require 'highlight-symbol)
;; (global-set-key [(control .)] 'highlight-symbol-at-point)
;; (global-set-key [(control \})] 'highlight-symbol-next)
;; (global-set-key [(control \{)] 'highlight-symbol-prev)
(require 'highlight-symbol)
(global-set-key (kbd "C-.") 'highlight-symbol-at-point)
(global-set-key (kbd "C-\}") 'highlight-symbol-next)
(global-set-key (kbd "C-\{") 'highlight-symbol-prev)
;; (global-set-key [(control \$)] 'highlight-symbol-next)
;; (global-set-key [(control \#)] 'highlight-symbol-prev)


;; This does not look useful 2013-08-05
;; auto-highlight-symbol.el for highlighting multiple occurences
;; https://github.com/emacsmirror/auto-highlight-symbol
;;
;; (require 'auto-highlight-symbol)
;; (global-auto-highlight-symbol-mode t)




;; multiple-cursors for simultaneous editing multiple occurrences
;; http://ongaeshi.hatenablog.com/entry/20121205/1354672102 (for a similar package)
;; http://emacsrocks.com/e13.html (video)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
;;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; word-count.el						; Does not work in emacs 24. use count-word
;; http://taiyaki.org/elisp/word-count/src/word-count.el
;; (setq load-path (cons (expand-file-name "~/elisp") load-path))
;; (autoload 'word-count-mode "word-count"
;;   "Minor mode to count words." t nil)
;; (global-set-key "\M-+" 'word-count-mode)	;; Turned by K. Conflict with other functions.
;; How to use:
;; 1). M-+ (word-count-mode) toggles word-count mode.
;; 2). M-[space] (word-count-set-area) sets area for counting words.
;; 3). M-x word-count-set-region sets region or paragraph for counting words.
;; 4). M-x word-count-set-marker sets marker for counting words.


;; e2wn		; Window management system
;; e2wm minimal configuration (requires window-layout.el)
;; http://d.hatena.ne.jp/kiwanami/20100528/1275038929
(require 'e2wm)
(global-set-key (kbd "M-+") 'e2wm:start-management)
;;
;; e2wn-R
;; http://sheephead.homelinux.org/2011/03/15/6687/
;;(require 'inlineR) automatically loaded
(require 'e2wm-R)


;; Undo tree
;; http://www.emacswiki.org/emacs/UndoTree
;; C-/ for undo. C-? (C-S-/) for redo.
(require 'undo-tree)
(global-undo-tree-mode)


;; point-undo
;; http://d.hatena.ne.jp/rubikitch/20081230/pointundo
(require 'point-undo)
;; (define-key global-map [f7] 'point-undo)
;; (define-key global-map [S-f7] 'point-redo)
(define-key global-map (kbd "<f7>") 'point-undo)
(define-key global-map (kbd "S-<f7>") 'point-redo)



;; go to last change
;; http://www.emacswiki.org/emacs/GotoChg
(require 'goto-chg)
(global-set-key (kbd "<f8>") 'goto-last-change)
(global-set-key (kbd "S-<f8>") 'goto-last-change-reverse)


;; sequential-command.el
;; Book by rubikitch p76. M-x auto-install-batch sequential-command (two files, one -config)
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(require 'sequential-command-config)	; config file
(sequential-command-setup-keys)		; Rebind C-a, C-e, M-u, M-c, and M-l to seq-* commands.
(global-set-key (kbd "C-a") 'seq-home)
(global-set-key (kbd "C-e") 'seq-end)
(global-set-key (kbd "M-u") 'seq-upcase-backward-word)
(global-set-key (kbd "M-c") 'seq-capitalize-backward-word)
(global-set-key (kbd "M-l") 'seq-downcase-backward-word)
;;
;; smartchr.el
;; Similar elips: http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)
;; http://ratememo.blog17.fc2.com/blog-entry-937.html
(defun my-ess-smartchr-setting ()
  (local-set-key (kbd "=") (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+") (smartchr '(" + " "+")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------"))) ; test
  (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################"))) ; test
  (local-set-key (kbd "~") (smartchr '(" ~ " "~")))
  (local-set-key (kbd "$") (smartchr '("$" "$`!!'$")))
  (local-set-key (kbd "%") (smartchr '("%" " %`!!'% ")))
;;  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "["))) ; not very useful
  ) 
(add-hook 'ess-mode-hook	  'my-ess-smartchr-setting)
(add-hook 'inferior-ess-mode-hook 'my-ess-smartchr-setting)	; 20130426 off did not solve ( issue
;; (global-set-key (kbd "=") (smartchr '(" = " " == " "=")))	; =
;; (global-set-key (kbd "(") (smartchr '("(`!!')" "(")))		; ()
;; (global-set-key (kbd "[") (smartchr '("[`!!']" "[")))		; []
;; (global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))		; {}
;; (global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))	; ""
;; (global-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'")))	; ''


;; bash-completion in emacs' shell (M-x shell)
;; http://stackoverflow.com/questions/163591/bash-autocompletion-in-emacs-shell-mode
;; https://github.com/szermatt/emacs-bash-completion
;;
;; http://www.namazu.org/~tsuchiya/elisp/shell-command.el
(require 'shell-command)
(shell-command-completion-mode)
;;
;; https://github.com/szermatt/emacs-bash-completion
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
	  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
	  'bash-completion-dynamic-complete)


;; open-junk-file.el for creation of permanent test files
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)


;; lispxmp.el to evaluate sexp within .el
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)


;; paredit.el
;; M-x install-elisp http://mumble.net/~campbell/emacs/paredit.el
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
;; (add-hook 'ess-mode-hook 'enable-paredit-mode)		; paredit for ESS. too restrictive
;; No space when inserted after a word
;; http://stackoverflow.com/questions/913449/changing-paredit-formatting
(defun paredit-space-for-delimiter-p (endp delimiter)
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\"  ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))


;; rainbow-delimiters.el
;; http://www.emacswiki.org/emacs/RainbowDelimiters
(require 'rainbow-delimiters)
(add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)	; for programming related modes
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2")))))


;; highlight-sexp.el
;; http://www.emacswiki.org/emacs/HighlightSexp
;; Color M-x list-colors-display  to check good colors
(require 'highlight-sexp)
;; (setq hl-sexp-background-color "thistle1")
;; (setq hl-sexp-background-color "snow1")
(setq hl-sexp-background-color "CadetBlue1")
;; (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
;; (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
;; (add-hook 'ess-mode-hook 'highlight-sexp-mode)	; Not turned on by default use sx to toggle


;; Auto byte-compile .el files at saving
;; http://www.emacswiki.org/emacs/auto-async-byte-compile.el
;; http://d.hatena.ne.jp/rubikitch/20100423/bytecomp
(require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|init.el")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; ;; Use default eldoc (loaded automatically)
;; (require 'eldoc)
;; eldoc-extension
;; http://d.hatena.ne.jp/rubikitch/20090207/1233936430
(require 'eldoc-extension)
(setq eldoc-idle-delay 0)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-minor-mode-string "")		; No need for ElDoc in modeline (rubikitch book p231)


;; Python support				; external dependency
;; Wiki: http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs
;; Ref
;; Jedi completion: http://www.masteringemacs.org/articles/2013/01/10/jedi-completion-library-python/
;; Python emacs endevavour: http://www.johndcook.com/blog/2013/01/29/python-emacs-setup/
;; emacs configuration (Japanese): http://zaikeyuki.blog96.fc2.com/blog-category-8.html
;;
;; python.el (comes with emacs)	; not as functional, use python-mode.el
;; http://jesselegg.com/archives/2010/02/25/emacs-python-programmers-part-1/
;; http://d.hatena.ne.jp/cou929_la/20110525/1306321857
;; $pip install pyflakes pep8 # This does not work
;;
;; python-mode.el (version 6.1.1 latest as of 2013-02-25)
;; https://launchpad.net/python-mode
;; http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
;; http://tech.lampetty.net/tech/index.php/archives/380
(add-to-list 'load-path "~/.emacs.d/plugins/python-mode.el-6.1.1/") 
(setq py-install-directory "~/.emacs.d/plugins/python-mode.el-6.1.1/")
;;
;; (setq py-shell-name "ipython")
(setq py-shell-name "/usr/local/bin/ipython")			; external dependency!!
(require 'python-mode)
;; http://stackoverflow.com/questions/8226493/ipython-emacs-integration
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;
;; Vertical split on execution
;; (setq py-split-windows-on-execute-function (quote split-window-horizontally))
(setq py-split-windows-on-execute-function (quote split-window-vertically))
(setq py-split-windows-on-execute-p t)
;;
;; IPython.el	; Required for completion?
;; http://ipython.org/ipython-doc/stable/config/editors.html
;; https://github.com/ipython/ipython/download
;; Downloaded source and installed in plugins folder
(setq ipython-command "/usr/local/bin/ipython")
(setq-default py-python-command-args '("--pylab" "--colors=LightBG"))
(require 'ipython)
;;
;; IPython completion
;; http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs#toc13
(setq ipython-completion-command-string
      "print(';'.join(get_ipython().Completer.complete('%s')[1])) #PYTHON-MODE SILENT\n")
;;
;; ;; PyFlakes syntax checking	; Does not work 2013-02-26 (external command not recognized?)
;; ;;
;; (require 'flymake)	; This is necessary first? http://d.hatena.ne.jp/yascentur/20130114/1358143181
;; ;; (set flymake-gui-warnings-enabled nil)	; http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
;; (defun flymake-python-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "pyflakes" (list local-file))))
;;
;; (defconst flymake-allowed-python-file-name-masks '(("\\.py$" flymake-python-init)))
;; (defvar flymake-python-err-line-patterns '(("\\(.*\\):\\([0-9]+\\):\\(.*\\)" 1 2 nil 3)))
;;  ;;
;; (defun flymake-python-load ()
;;   (interactive)
;;   (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;     (setq flymake-check-was-interrupted t))
;;   (ad-activate 'flymake-post-syntax-check)
;;   (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-python-file-name-masks))
;;   (setq flymake-err-line-patterns flymake-python-err-line-patterns)
;;   (flymake-mode t))
;; (add-hook 'python-mode-hook '(lambda () (flymake-python-load)))
;;
;; Jedi for auto-completion	; 2013-02-26
;; http://tkf.github.com/emacs-jedi/
;; https://jedi.readthedocs.org/en/latest/docs/installation.html ; sudo easy_install virtualenv jedi epc needed
;; (add-hook 'python-mode-hook 'jedi:setup)	; Turned off 2013-08-09
;;
;; ein.el	; Emacs IPython Notebook
;; http://tkf.github.com/emacs-ipython-notebook/
;; (require 'ein)
;; Usage
;; Start IPython notebook server.
;; Hit M-x ein:notebooklist-open to open notebook list. This will open notebook list buffer.
;; In the notebook list buffer, you can open notebooks by hitting [Open] buttons. See notebook section for what you can do in the notebook buffer.
;;
;; elpy.el
;; https://github.com/jorgenschaefer/elpy/wiki
;; Python side installation for elpy.el
;; First, you need the elpy package itself:
;; pip install elpy
;; Then you need to decide on whether you want to use rope or jedi, and install either one:
;; pip install rope
;; pip install jedi
;; To use the syntax highlighting capabilities, install two more packages:
;; pip install pyflakes pep8
;;
;; To use elpy, just add the following to your .emacs:
(package-initialize)
(elpy-enable)
;; If you want to use IPython (make sure it's installed), add:
(elpy-use-ipython)
;; If you find the (Python Elpy yas AC ElDoc Fill) mode line annoying, also add:
(elpy-clean-modeline)
;;
;; Fix yas-snippet-dirs (elpy breaks configuration)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
	"/Users/kazuki/.emacs.d/elpa/yasnippet-20130722.1832/snippets"
	))


;; icicles.el		; 2013-08-09 Turned off more problems than solutions
;; http://www.emacswiki.org/emacs/Icicles
;; http://www.emacswiki.org/emacs/EmacsNewbieWithIcicles
(require 'icicles)
(icy-mode 1)



;; Auto-completion addtional setting
;; Looks like this has to come after ESS configuration
(define-key ac-completing-map (kbd "RET") 'ac-complete)


;; This is placed at the end as it freezes if EmacsWIki is not responding.
(auto-install-update-emacswiki-package-name t) ; THIS FREEZES EMACS IF EMACSWIKI IS DONW!!!!!!
;;; end of configuration
