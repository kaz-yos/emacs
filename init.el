;;
;; ~/.emacs.d/init.el for cocoa emacs 24.3.1 (source with inline patch) on Mac OS X 10.8.4
;; Reference: http://sakito.jp/emacs/emacs24.html#ime
;;

;;
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

  ;; Do not open a new frame opening a file from Finder
  ;; http://stackoverflow.com/questions/6068819/alias-to-make-emacs-open-a-file-in-a-new-buffer-not-frame-and-be-activated-com
  (setq ns-pop-up-frames nil)
  )
;
;; exec-path-from-shell.el
;; This could be used instead of the code above 
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))


;;
;;; Configuration without external .el file dependencies comes first ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


;;; default.el in Vincent Goulet's distribution
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/Emacs-23.3-modified-3/default.el
;;
;; Nice options to have On by default
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


;; Suppress all dialog boxes completely, even file open dialogue. No need for mouse!
;; http://www.gnu.org/s/libtool/manual/emacs/Dialog-Boxes.html
(setq use-dialog-box nil)


;;; Use Mac OS X system trash
;; http://www.masteringemacs.org/articles/2010/12/30/making-deleted-files-trash-can/
;; http://www.reddit.com/r/emacs/comments/iuyef/emacs_on_mac/
(when (eq system-type 'darwin)
  ;; Mac-only
  (setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs")
  )


;;; dired configuration
;; Show sizes in KB/MB etc
;; http://unix.stackexchange.com/questions/44858/change-view-in-emacs-dired-mode
(setq dired-listing-switches "-ahl")


;;; Bars: Menu bar only. No scroll bar or tool bar.
;; http://www.emacswiki.org/emacs/FullScreen#toc7
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(scroll-bar-mode t)


;; Show the current directory in the frame bar
;; http://stackoverflow.com/questions/8945056/emacs-how-to-show-the-current-directory-in-the-frame-bar
(setq frame-title-format '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))


;; winner-mode	
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


;;; Window management
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
(global-set-key (kbd "<C-tab>") 'other-window-or-split)
;; Reversal
;; http://stackoverflow.com/questions/143072/in-emacs-what-is-the-opposite-function-of-other-window-c-x-o
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
(global-set-key (kbd "C-M-t")		'scroll-other-window-down)
(global-set-key (kbd "C-M-v")		'scroll-other-window)


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


;; ffap find-file-at-point
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


;;; Keyboard modification
;;
;; C-h for delete, C-? for help
;; http://www.emacswiki.org/emacs-en/BackspaceKey
;; Use M-x help for help
(define-key key-translation-map [?\C-h] [?\C-?])
;;
(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Command key as Meta key, Option key untouched
  ;; http://www.emacswiki.org/emacs/MetaKeyProblems#toc15
  ;;
  ;; Remove default both options to meta binding
  (setq mac-option-modifier nil)
  ;; right option as meta
  (setq mac-right-option-modifier 'meta)
  ;; left option as super
  (setq mac-left-option-modifier 'super)
  ;; command as meta
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


;; Increase max values for number of variables that can be defined 2013-09-19
;; http://mikio.github.io/article/2012/06/26_variable-binding-depth-exceeds-max-specpdl-size.html
(setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 6000)


;; CUA		Rectangle is not used. 
;; Common User Access mode for column editing: Activated by C-RET while selecting text
;; http://tech.kayac.com/archive/emacs-rectangle.html
;; http://trey-jackson.blogspot.com/2008/10/emacs-tip-26-cua-mode-specifically.html
;; http://stackoverflow.com/questions/3750332/how-do-i-force-a-binding-in-emacs
(setq cua-rectangle-mark-key (kbd "<C-S-return>")) ; <C-S-return> for rectangle
(cua-mode t)
(setq cua-enable-cua-keys nil)			; C-x C-c C-v left intact
;; (setq cua-keep-region-after-copy t)		; Keep selection after copying (Mac/Win-like)


;; No auto filling in text mode
;; http://tomikura.s2.xrea.com/linux/install/emacs.html
(setq fill-column 80)
(setq text-mode-hook '(lambda () (auto-fill-mode 0)))
;;(setq default-major-mode 'text-mode)	; Obsolete as of version 23.2
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html
(setq-default major-mode 'text-mode)


;; C-RET for eval-region in elisp mode 2013-12-22
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-region)


;;; Language settings
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
;; ln -s /Applications/Emacs24.3.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient
;; .profile: export EDITOR="emacsclient"
;; http://www.emacswiki.org/emacs/EmacsClient
(require 'server)
(unless (server-running-p)
  (server-start))
(server-start)		; Start server
(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))



;;; Automatically close brackets and parentheses
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



;; Additional configurations
;;
;; Take current line to top
(defun my-recenter-top ()
  (interactive)
  (recenter "Top")
)
(global-set-key (kbd "C-S-l") 'my-recenter-top)
;;
;; http://ergoemacs.org/emacs/elisp_datetime.html
(defun my-insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert (format-time-string "%Y-%m-%d"))
  )
(global-set-key (kbd "C-c d") 'my-insert-date)
;;
(global-set-key (kbd "C-c r") 'replace-string)
;;
(global-set-key (kbd "C-c m") 'comment-region)


;; delay linum for speed
;; http://d.hatena.ne.jp/daimatz/20120215/1329248780
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))


;;; Surround region
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


;;; Package system and MELPA
;; http://www.emacswiki.org/emacs/ELPA
(require 'package)
;;
;; Initialize 2013-10-31 moved ahead of repositories
(package-initialize)
;;
;; MELPA	; 2013-10-31 Manually running this line fixed it? corrupt temporary file??
;; http://melpa.milkbox.net/#installing
;; http://melpa.milkbox.net/#/getting-started
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; Marmalade
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



;;; Buffer Management
;;
;; ibuffer
;; http://www.emacswiki.org/emacs/IbufferMode
;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)
;;
;; Switching to ibuffer puts the cursor on the most recent buffer	; Not useful 2014-01-25
;; http://www.emacswiki.org/emacs/IbufferMode#toc13
;; (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
;;   "Open ibuffer with cursor pointed to most recent buffer name"
;;   (let ((recent-buffer-name (buffer-name)))
;;     ad-do-it
;;     (ibuffer-jump-to-buffer recent-buffer-name)))
;; (ad-activate 'ibuffer)
;;
;; Emacs Tip of the Day: Start Using IBuffer ASAP.
;; http://mytechrants.wordpress.com/2010/03/25/emacs-tip-of-the-day-start-using-ibuffer-asap/
;; (setq ibuffer-default-sorting-mode 'major-mode)
;; https://github.com/pd/dotfiles/blob/master/emacs.d/pd/core.el
(setq ibuffer-default-sorting-mode 'filename/process	; Sort by filename/process
      ibuffer-show-empty-filter-groups nil)		; Don't show empty groups
;;
;; Add git support
(require 'ibuffer-git)
;; Choose the column width for the long status
;; (setq ibuffer-git-column-length 8)	; default is 8.
;; git-status-mini (git-status 8 8 :left) are defined. Add to ibuffer-formats
;;
;; Modify the default ibuffer-formats	; git support and size formatting
;; http://unix.stackexchange.com/questions/35830/change-column-width-in-an-emacs-ibuffer-on-the-fly
(setq ibuffer-formats
      '((mark modified read-only	; Three flags without spaces in between.
	      " "			;
	      (name 18 18 :left :elide)	; Buffer name
	      " "			;
	      git-status-mini		; ibuffer-git short status
	      " "			;
	      (size-h 9 -1 :right)	; size-h defined at the end
	      ;;(size 9 -1 :right)	; original size in bytes
	      " "			;
	      (mode 10 10 :left :elide)	; Mode
	      " "
	      filename-and-process)))
;;
;; Classify
;; http://www.emacswiki.org/emacs/IbufferMode#toc6
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("DIRED" (mode . dired-mode))
	       ("EMACS" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Packages\\*$")
			 ))
	       ("ESS"   (or
			 (mode . ess-mode)
			 (mode . inferior-ess-mode)))
	       ("PYTHON" (or
			  (mode . python-mode)
			  (mode . inferior-python-mode)))
	       ("SHELL"  (or
			  (mode . sh-mode)
			  (mode . shell-mode)
			  (mode . ssh-mode)
			  (mode . eshell-mode)))
	       ("SQL"  (or
			  (mode . sql-mode)
			  (mode . sql-interactive-mode)))
	       ("LISP"	(or 
			 (mode . emacs-lisp-mode)))
	       ("TeX"    (or
			  (mode . TeX-mode)
			  (mode . LaTeX-mode)))
	       ("MAGIT"  (or 
			  (mode . magit-mode)
			  (mode . magit-status-mode)
			  (mode . magit-process-mode)
			  (mode . git-commit-mode)))
	       ))))
;; Group for the other buffers.
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
;;
;; Use human readable Size column instead of original one 2014-01-15
;; http://www.emacswiki.org/emacs/IbufferMode#toc12
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))
;;



;;; Mac-only configuration
(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Change to English in minibuffer (require inline patch. No .el dependency)
  ;; http://molekun.blogspot.com/2011/03/homebrewemacs233.html
  ;; http://blog.n-z.jp/blog/2013-11-12-cocoa-emacs-ime.html
  (when (fboundp 'mac-change-language-to-us)	; Only when inline patch is installed 2014-01-19
    (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
    )


  ;; Spell checker (require aspell. No .el dependency)
  ;; http://blog.bungu-do.jp/archives/2426
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
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil))))


;; minor-mode-hack.el
;; M-x show-minor-mode-map-priority to see minor mode priority
(require 'minor-mode-hack)


;; show-keys.el
;; http://www.youtube.com/watch?v=0cZ7szFuz18
;; https://github.com/AndreaCrotti/minimal-emacs-configuration
(require 'show-keys)


;;; bm.el	Within-file bookmarking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;
;; Define function to do bm-previous/next and recenter
(defun my-bm-next ()
  (interactive)
  (bm-next)
  (recenter "Top"))
(defun my-bm-previous ()
  (interactive)
  (bm-previous)
  (recenter "Top"))
;;
;; Keyboard
(global-set-key (kbd "M-SPC")	'bm-toggle)	; Conflict with IM. Use ESC-SPC, which is the same
;; (global-set-key (kbd "M-]")	'bm-next)
;; (global-set-key (kbd "M-[")	'bm-previous)
(global-set-key (kbd "M-]")	'my-bm-next)
(global-set-key (kbd "M-[")	'my-bm-previous)


;; windresize for M-x windresize
;; M-x windresize, arrows, C-g for cancel, RET to save
(require 'windresize)



;;; Templates by autoinsert.el
;;
;; autoinsert.el
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/autoinsert/")
(define-auto-insert "\\.Rmd$" "knitr.Rmd")
(define-auto-insert "\\.Rnw$" "knitr.Rnw")
(define-auto-insert "\\.sas$" "SAS.sas")
(define-auto-insert "\\.sh$" "shell.sh")
(define-auto-insert "\\.tex$" "LaTeX.tex")
(define-auto-insert "\\.gitignore$" ".gitignore")


;;; Auto-completion
;;
;; auto-complete.el, auto-complete-config.el, fuzzy.el, popup.el downloaded from below URL
;; https://github.com/auto-complete/auto-complete
;; http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
;;
;; Auto-complete for ESS configuration
;; http://www.emacswiki.org/emacs/ESSAuto-complete 
(setq 
 ;; ac-candidate-limit nil
 ac-delay 0			; Faster than default 0.1 before AC kicks in
 ac-auto-show-menu 0.1		; 0.1 sec before menu appears
 ac-candidate-menu-min 1	; Show menu if 2+ candidates
 ac-menu-height 20		; 20 candidates
 ;; http://stackoverflow.com/questions/17309773/emacs-autocomplete-inside-python-string
 ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
 ac-disable-faces nil		; auto-complete everywhere, even within quotes, comments
 ;; ac-ignore-case 'smart	; Treat them smartly
 ac-ignore-case nil		; Treat them strictly
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
;;
;; If you are using 'flyspell' you might want to activate the workaround
;; http://www.emacswiki.org/emacs/AutoComplete#toc6
(ac-flyspell-workaround)
;;
;; auto-complete-emacs-lisp.el 2013-09-08
;; https://github.com/rik0/tentative-configuration-emacs/blob/master/emacs.d/auto-complete-emacs-lisp.el
(require 'auto-complete-emacs-lisp)
;;
;; popup.el
;; https://github.com/auto-complete/popup-el (called automatically)
;; (require 'popup)	
;; Prevent broken popup
;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
(setq popup-use-optimized-column-computation nil)


;; icicles.el
;; http://www.emacswiki.org/emacs/Icicles
;; http://www.emacswiki.org/emacs/EmacsNewbieWithIcicles
(require 'icicles)
(icy-mode 1)


;;; flyspell-mode 2014-01-20
;; Define a fuction to use the popup.el
;; http://d.hatena.ne.jp/mooz/20100423/p1
(defun flyspell-correct-word-popup-el ()
  "Pop up a menu of possible corrections for misspelled word before point."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
	(word (flyspell-get-word nil)))
    (if (consp word)
	(let ((start (car (cdr word)))
	      (end (car (cdr (cdr word))))
	      (word (car word))
	      poss ispell-filter)
	  ;; now check spelling of word.
	  (ispell-send-string "%\n")	;put in verbose mode
	  (ispell-send-string (concat "^" word "\n"))
	  ;; wait until ispell has processed word
	  (while (progn
		   (accept-process-output ispell-process)
		   (not (string= "" (car ispell-filter)))))
	  ;; Remove leading empty element
	  (setq ispell-filter (cdr ispell-filter))
	  ;; ispell process should return something after word is sent.
	  ;; Tag word as valid (i.e., skip) otherwise
	  (or ispell-filter
	      (setq ispell-filter '(*)))
	  (if (consp ispell-filter)
	      (setq poss (ispell-parse-output (car ispell-filter))))
	  (cond
	   ((or (eq poss t) (stringp poss))
	    ;; don't correct word
	    t)
	   ((null poss)
	    ;; ispell error
	    (error "Ispell: error in Ispell process"))
	   (t
	    ;; The word is incorrect, we have to propose a replacement.
	    (flyspell-do-correct (popup-menu* (car (cddr poss)) :scroll-bar t :margin t)
				 poss word cursor-location start end cursor-location)))
	  (ispell-pdict-save t)))))

;; C-M-return at the word to correct to activate
(add-hook 'flyspell-mode-hook
          (lambda ()
            (define-key flyspell-mode-map (kbd "<C-M-return>") 'flyspell-correct-word-popup-el)
            ))

;; Auto-start flyspell-mode for these files
;; (add-to-list 'auto-mode-alist '("\\.txt" . flyspell-mode))
;; (add-to-list 'auto-mode-alist '("\\.tex" . flyspell-mode))

;; Save word without a mouse
;; http://stackoverflow.com/questions/11070849/flyspell-without-a-mouse
(defun save-ispell-word (word)
  (interactive "sA word you want to add to dictionary ")
  (ispell-send-string (concat "*" word "\n"))
  (setq ispell-pdict-modified-p '(t)))


;; ac-ispell	; 2014-01-20
;; Auto-completion for English words
;; (custom-set-variables
;;   '(ac-ispell-requires 4))

;; (eval-after-load "auto-complete"
;;   '(progn
;;       (ac-ispell-setup)))

;; (defun my/enable-ac-ispell ()
;;   (add-to-list 'ac-sources 'ac-source-ispell))

;; (add-hook 'git-commit-mode-hook 'my/enable-ac-ispell)
;; (add-hook 'mail-mode-hook 'my/enable-ac-ispell)




;;; YAsnippet ; Automatic template inserting system.
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
;;
;; Use Popup isearch For Yasnippet Prompt
;; http://iany.me/2012/03/use-popup-isearch-for-yasnippet-prompt/
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))


;;; Emacs Speaks Statistics (ESS) for emacs
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
;; Now installed via elpa
;;
;; Some condigurations were taken from vgoulet Emacs distribution configuration files
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/
(require 'ess-site)
(require 'ess-eldoc)				; Slows cursor movements slightly?
;; (setq ess-eldoc-show-on-symbol t)		; Shows eldoc when cursor is on function name (Causes errors)
;;
;; Installed via el-get. add functionalities to ESS. Error? 2013-08-20
;; (require 'ess-edit)
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
;; R buffer name manipulation
;; https://stat.ethz.ch/pipermail/ess-help/2012-December/008426.html
;; http://t7331.codeinpro.us/q/51502552e8432c0426273040
;; (setq ess-gen-proc-buffer-name-function (lambda (proc) (concatenate 'proc "R")))
;; (setq ess-gen-proc-buffer-name-function (lambda (proc) "nameR"))
;;
;; Starting R on the left
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
	;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Selecting-Windows.html
	(select-window w2)		; Select script (w2) Added
	)))
;;
(defun my-ess-eval-R ()
  (interactive)
  (my-ess-start-R)			; R starts on the right if this is not used.
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
	     ;; my-ess-eval-R
	     (local-set-key (kbd "<S-return>") 'my-ess-eval-R)
	     (local-set-key (kbd "<C-return>") 'my-ess-eval-R)	; Change to my-ess-eval-R
	     ;; Toggling $ in S-syntax-table
	     (local-set-key (kbd "C-c 4") 'toggle-dollar)	; Toggle $ in S-syntax-table
	     (modify-syntax-entry ?$  " "  S-syntax-table)	; $ as whitespace in S
	     ;; Additional keybinds
	     ;; (local-set-key (kbd "C-c n") 'ess-next-code-line) ; Not useful?
	     ;; (local-set-key (kbd "M-n p") 'ess-swv-PDF)	  ; Does not work. M-n is prefix. 2013-09-08
	     ))
;;
(add-hook 'inferior-ess-mode-hook	; For iESS mode
          '(lambda()
	     (local-set-key (kbd "C-c w") 'ess-execute-screen-options)	; To adjust width
             ;; (local-set-key [C-up] 'comint-previous-input)
             ;; (local-set-key [C-down] 'comint-next-input)
             (local-set-key (kbd "C-<up>") 'comint-previous-input)
             (local-set-key (kbd "C-<down>") 'comint-next-input)
	     (local-set-key (kbd "C-c 4") 'toggle-dollar)	; Toggle $ in S-syntax-table
	     ;; (modify-syntax-entry ?$  "_"  S-syntax-table)	; $ as symbol in iESS. not working
	     ))
;; https://stat.ethz.ch/pipermail/ess-help/2009-July/005455.html
(add-hook 'ess-post-run-hook
	  '(lambda ()
	     (ess-execute-screen-options)))			; Reset screen width
;;
(add-hook 'Rnw-mode-hook		; For Rnw mode
          '(lambda()
	     (local-set-key (kbd "<S-return>") 'my-ess-eval-R)
	     ))
;;
;; ess-trace-bug.el		; filtering ++++ > ??? Not working
;; http://code.google.com/p/ess-tracebug/
;; (require 'ess-tracebug)	; Now included in ESS
(setq ess-use-tracebug t)	; permanent activation
;;
;; *.Rmd files invoke r-mode	; Temporary fix for R markdown files
(setq auto-mode-alist
      (cons '("\\.Rmd$" . r-mode) auto-mode-alist))
;; *.R files invoke read-only-mode for protection (This prevents ESS mode)
;; (setq auto-mode-alist
;;       (cons '("\\.R$" . read-only-mode) auto-mode-alist))
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
;; Reproducible research with knitr, etc
;; Use knitr for .Rnw document
(setq ess-swv-processor 'knitr)
;; Add commands to AUCTeX's M-x TeX-command-list
(setq ess-swv-plug-into-AUCTeX-p t)
;; Supress ess-swv-PDF from opening PDF by meaning less value
;; http://tex.stackexchange.com/questions/69660/sweave-how-to-suppress-opening-new-instance-of-a-pdf-when-running-pdflatex
(setq ess-pdf-viewer-pref "ls")
;; Define a one step function for .Rnw 2013-09-10
(defun ess-swv-weave-PDF()
  (interactive)
  (ess-swv-weave nil)	; nil to run with default processor
  (ess-swv-PDF "texi2pdf")
  )
(add-hook 'LaTeX-mode-hook
	  '(lambda()
	     (local-set-key (kbd "C-c e") 'ess-swv-weave-PDF)
	     ))
;;
;; inlineR.el for graphics inside code
;; http://sheephead.homelinux.org/2011/02/10/6602/
;; https://github.com/myuhe/inlineR.el
(require 'inlineR)
;; (setq inlineR-re-funcname "plot\|image\|hogehoge\|my-func")	; recognize these plotting functions
;; (setq inlineR-default-image "jpeg")				; jpeg
;; (setq inlineR-default-dir "/tmp/")				; images in tmp dir
;; (setq inlineR-cairo-p t)					; Use Cairo package
;;
;; cacoo.el for additional graphics functionalities (need account?)
(require 'cacoo)
(require 'cacoo-plugins)      ; option
(setq cacoo:api-key "APIKEY") ; option
(global-set-key (kbd "M--") 'toggle-cacoo-minor-mode) ; key bind example
;;
;;
;; ESS julia language
;; https://github.com/emacs-ess/ESS/wiki/Julia
;; excecutable file Changed as of 2013-12-20
(setq inferior-julia-program-name "/Applications/Julia.app/Contents/Resources/julia/bin/julia-basic")
;; Define a function to starting julia
(defun my-ess-start-julia ()
  (interactive)
  (if (not (member "*julia*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (julia)
        (set-window-buffer w1 "*julia*")	; julia on the left (w1)
        (set-window-buffer w2 w1name)	; script on the right (w2)
	(select-window w2)		; Select script (w2) Added
	)))
;; Define a function to eval julia code	; Actually the one for R works???
(defun my-ess-eval-julia ()
  (interactive)
  (my-ess-start-julia)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
;; Hook for julia-mode
(add-hook 'julia-mode-hook 
	  '(lambda()
	     (local-set-key (kbd "<S-return>") 'my-ess-eval-julia)
	     (local-set-key (kbd "<C-return>") 'my-ess-eval-julia)
	     )
	  )
;;
;;
;; ESS SAS configuration
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
(add-hook 'sas-mode-hook	; For SAS mode
          '(lambda()
             (local-unset-key [C-tab] 'ess-sas-backward-delete-tab)))	; Unset C-tab from ESS major mode


;; STAN support 2014-01-15
(require 'stan-mode)
;; stan-snippets.el
;; (require 'stan-snippets)
;; flymake-stan.el
;; (require 'flymake-stan)



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


;;; shell environments
;;
;; essh.el for shell (like ESS for R
;; http://www.emacswiki.org/emacs/essh.el
;; Obsolete
;; http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
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
             (local-set-key (kbd "S-<return>") 'my-essh-eval)
	     (local-set-key (kbd "C-<return>") 'my-essh-eval)))

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

;; multi.term.el
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; flymake-shell.el 2013-12-27
;; https://github.com/purcell/flymake-easy
(require 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)


;; SSH support 2014-01-15
;; Support for remote logins using `ssh'.
;; This program is layered on top of shell.el; the code here only accounts
;; for the variations needed to handle a remote process, e.g. directory
;; tracking and the sending of some special characters.
;;
;; If you wish for ssh mode to prompt you in the minibuffer for
;; passwords when a password prompt appears, just enter m-x send-invisible
;; and type in your line, or add `comint-watch-for-password-prompt' to
;; `comint-output-filter-functions'.
(require 'ssh)
;;
;; Completion. Not functional as of 2014-01-18.
;; https://github.com/ieure/ssh-el
(add-hook 'ssh-mode-hook
	  (lambda ()
	    (setq ssh-directory-tracking-mode 'ftp)	; Using the function with the same name is better?
	    (shell-dirtrack-mode t)))


;; Shell Dirtrack By Prompt 2014-01-17. Not functional yet
;; http://www.emacswiki.org/emacs/ShellDirtrackByPrompt
;; http://nflath.com/2009/09/dirtrack-mode/
;; First, program your shell prompt to emit the PWD
;; ~/.bashrc
;; if [ $EMACS ]; then
;;     # Emit the PWD in the prompt, taking care that it doesn't get truncated.
;;     PS1='\n\u@\h:$(pwd) \n\$ '
;; fi
;; Capture 
;;  
;; (require 'dirtrack)	; Check if this is necessary for eshell.
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;; 	     ;; List for directory tracking.
;; 	     ;; First item is a regexp that describes where to find the path in a prompt.
;; 	     ;; Second is a number, the regexp group to match.
;; 	     ;; http://stackoverflow.com/questions/15812638/emacs-i-need-and-explanation-on-dirtrack-list-variable
;; 	     ;; (setq dirtrack-list '("^[^@]*@[^ ]*:\([^\]]*\) \$" 1))
;; 	     (setq dirtrack-list '("^[^@]*@\\([^:]*:[^$].*\\) \\$" 1))
;; 	     (dirtrack-mode 1)
;; 	     (shell-dirtrack-mode nil)
;; 	     ))
;;
;; This is probably about ssh'ing into a remote computer and using the emacs there.
;; http://nflath.com/2009/09/dirtrack-mode/
;; (add-hook 'dirtrack-directory-change-hook
;;           (lambda ()
;;             (let ((base-buffer-name (concat "shell-" default-directory "-shell"))
;;                   (i 1)
;;                   (full-buffer-name base-buffer-name))
;;               (while (get-buffer full-buffer-name)
;;                 (setq i (1+ i))
;;                 (setq full-buffer-name (concat base-buffer-name "<" (number-to-string i) ">")))
;;               (rename-buffer full-buffer-name))))
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (shell-dirtrack-mode -1)
;;             (insert "export PS1=\"nflath@/:\\w$ \"")
;;             (comint-send-input)
;;             (dirtrack-mode 1)
;;             ))

;; TRAMP (Transparent Remote Access, Multiple Protocols) 2014-01-17
;; Access remote files like local files
;; http://www.emacswiki.org/emacs/TrampMode
(require 'tramp)
(setq tramp-default-method "ssh")
;; Handle Odyssey's two-step authentication. Password:, then, Verification code:   2014-01-18
;; http://emacs.1067599.n5.nabble.com/emacs-hangs-when-connecting-from-windows-to-linux-with-tcsh-shell-td244075.html
;; (setq tramp-password-prompt-regexp "^.*\\([pP]assword\\|[pP]assphrase\\).*: ? *")	; Original
(setq tramp-password-prompt-regexp "^.*\\([pP]assword\\|[pP]assphrase\\|Verification code\\).*: ? *")
;;
;; Completion works in the eshell open from within a tramp connection
;; http://stackoverflow.com/questions/1346688/ssh-through-emacs-shell



;;; TeX environments
;;
;; AUCtex
;; http://www.gnu.org/software/auctex/index.html
;; http://www.gnu.org/software/auctex/manual/auctex.html
;;
;; Bare minimum
;; http://www.gnu.org/software/auctex/manual/auctex.html#Quick-Start
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;;
;; http://emacsworld.blogspot.com/2011/05/auctex-tip-automatically-save-file.html
(setq TeX-save-query nil) ;;autosave before compiling
;;
;; TeX-fold-mode on by default (C-c C-b C-o to actually fold
;; http://tex.stackexchange.com/questions/52179/what-is-your-favorite-emacs-and-or-auctex-command-trick
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (TeX-fold-mode 1)))
;;
;; Avoid font size changes 2013-10-14
;; http://stackoverflow.com/questions/9534239/emacs-auctex-latex-syntax-prevents-monospaced-font
;; Only change sectioning colour
(setq font-latex-fontify-sectioning 'color)
;; super-/sub-script on baseline
(setq font-latex-script-display (quote (nil)))
;; Do not change super-/sub-script font

;; Exclude bold/italic from keywords
(setq font-latex-deactivated-keyword-classes
    '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))
;;
;;
;; Japanese setting etc
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX
;; Hiragino font settings. Done in shell
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?Mac#i9febc9b
;;
;; Apple Script to update PDF in Preview.app 2013-09-28 currently not active
;; ~/scripts/refresh-preview.scpt
;; tell application "Preview" to activate
;; tell application "Emacs" to activate
;;
;; Add commands
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (add-to-list 'TeX-command-list
                                   '("Preview" "/usr/bin/open -a Preview.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Preview"))
                      (add-to-list 'TeX-command-list
                                   '("Skim" "/usr/bin/open -a Skim.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Skim"))
                      (add-to-list 'TeX-command-list
                                   '("displayline" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %s.pdf \"%b\""
                                     TeX-run-discard-or-function t t :help "Forward search with Skim"))
		      ;; TeXShop for PDF
		      ;; Source - Configure for External Editor
		      ;; Preview - Automatic Preview Update
                      (add-to-list 'TeX-command-list
                                   '("TeXShop" "/usr/bin/open -a TeXShop.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run TeXShop"))
                      (add-to-list 'TeX-command-list
                                   '("TeXworks" "/usr/bin/open -a TeXworks.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run TeXworks"))
                      (add-to-list 'TeX-command-list
                                   '("Firefox" "/usr/bin/open -a Firefox.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Mozilla Firefox"))
                      (add-to-list 'TeX-command-list
                                   '("AdobeReader" "/usr/bin/open -a \"Adobe Reader.app\" %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Adobe Reader"))
		      )))
;;
;; (setq TeX-default-mode 'japanese-latex-mode)
;; jsarticle as the default style
(setq japanese-LaTeX-default-style "jsarticle")
;; For Japanese document
(setq kinsoku-limit 10)
;;
;; Engines
;; (setq TeX-engine-alist '((ptex "pTeX" "eptex" "platex" "eptex")
;;                          (uptex "upTeX" "euptex" "uplatex" "euptex")))
;; Select TeX engine from default(pdfTeX) luatex omega ptex uptex xetex
;; (setq TeX-engine 'ptex)
;;
;; Japanese setting by Dr. Okumura (not used for now
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX#h32722ec
;;
;; TeX-PDF mode (no DVI intermediate file in pdfTeX, LuaTex, XeTeX)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;;
;; auctex-latexmk.el for Japanese tex to PDF direct conversion: 
;; http://qiita.com/tm_tn/items/cbc813028d7f5951b165
;; https://github.com/tom-tan/auctex-latexmk/
;; ln -sf ~/Documents/.latexmkrc ~/.latexmkrc
(require 'auctex-latexmk)
(auctex-latexmk-setup)
;;
;; Interactive mode for errors
(add-hook 'LaTeX-mode-hook 'TeX-interactive-mode)
;;
;; Single-step compilation that works with Japanese	; 2013-09-28 removed
;; http://miyazakikenji.wordpress.com/2013/07/10/ess-で-sweave-出力/
;; http://stackoverflow.com/questions/11060023/ess-auctex-sweave-synctex-integration-from-rnw-pdfviewer
;;
;;
;; Auto-complete for LaTeX
;;
;; ac-math.el: auto-complete sources for input of mathematical symbols and latex tags
;; https://github.com/vitoshka/ac-math#readme
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
;;
;; ac-latex-mode.el
(defun add-ac-source-words-in-same-mode-buffers () ; add back
  (setq ac-sources
        (append '(ac-source-words-in-same-mode-buffers)
                ac-sources))
  )
;;
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
	(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		ac-sources))
  )
;; Load by hook
(add-hook 'LaTeX-mode-hook (lambda ()
			     ;; For ac-latex-mode.el Overwrite default in LaTeX mode
			     (setq ac-sources '(ac-source-math-latex ac-source-latex-commands))
			     ;; (ac-l-setup) ; For auto-complete-latex (overwrite ac-sources)
			     ;; (ac-latex-mode-setup) ; For ac-math (add to ac-sources)
			     ;; Add back text completion.
			     ;; (add-ac-source-words-in-same-mode-buffers) ; Slow 2013-09-15 2013-10-12 not helpful
			     (local-set-key (kbd "M-p") 'ess-nuke-trailing-whitespace)
			     ))
;;
;; auto-complete-auctex.el 2013-09-09
;; https://github.com/monsanto/auto-complete-auctex/blob/master/auto-complete-auctex.el
;; Directly installed from github without packaging system
;; (require 'auto-complete-auctex)
;;
;; ;; auto-complete-latex.el 2013-09-09. Not useful turned off on 2013-10-12
;; ;; (available on el-get but requires hg (mercurial)?)
;; ;; https://bitbucket.org/tequilasunset/auto-complete-latex/src
;; ;; http://d.hatena.ne.jp/tequilasunset/20100202/p1
;; ;; http://d.hatena.ne.jp/tequilasunset/20100317/p1
;; ;; http://d.hatena.ne.jp/whitypig/20110908/1315477128
;; ;; http://keisanbutsuriya.blog.fc2.com/blog-entry-59.html
;; ;; $ hg clone https://bitbucket.org/tequilasunset/auto-complete-latex # installed hg from brew
;; (require 'auto-complete-latex)
;; (setq ac-l-dict-directory "~/.emacs.d/auto-install/ac-l-dict/") ; Manually created here
;; ;; (add-to-list 'ac-modes 'foo-mode)
;; ;; (add-hook 'LaTeX-mode-hook 'ac-l-setup) ; 2013-09-10 Configured in ac-math settings
;; ;;
;; ;;       SYMBOL |           MEANING
;; ;;      --------+----------------------------------
;; ;;         l    | LaTeX or pLaTeX
;; ;;         a    | AMS packages
;; ;;         b    | beamer
;; ;;         h    | hyperlinks
;; ;;         g    | graphics
;; ;;         m    | math sign or equations
;; ;;         c    | colors
;; ;;         t    | tables
;; ;;         f    | fonts
;; ;;         p    | unclassified external packages
;; ;;         F    | file names in a current directory
;; ;;         L    | label names
;; ;;         B    | bib keys
;; ;;         u    | user-commands or user-arguments
;;
;;
;; latex-math-preview.el 2013-09-08
;; http://www.emacswiki.org/emacs/LaTeXMathPreview
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
;; Paths to required external software (specific to MacTeX)
(setq latex-math-preview-command-path-alist
      '((latex . "/usr/texbin/latex")
	(dvipng . "/usr/texbin/dvipng")
	(dvips . "/usr/texbin/dvips")
	(pdflatex . "/usr/texbin/pdflatex")	; for beamer preview
	(gs . "/usr/local/bin/gs")		; for beamer preview
	))
;; Colors for dark background 2013-09-28
(setq latex-math-preview-dvipng-color-option nil)
(setq latex-math-preview-image-foreground-color "black")
(setq latex-math-preview-image-background-color "white")
;; Function to preview math expression and shift focus after preview
(defun my-latex-math-preview ()
  "Preview a math expression and shift focus after preview"
  (interactive)
  (setq w1 (selected-window))
  (latex-math-preview-expression)
  (select-window w1)
  )
;; (define-key LaTeX-mode-map (kbd "C-c m") 'my-latex-math-preview)
;; Function to preview Beamer slide and shift focus after preview
(defun my-latex-beamer-preview ()
  "Preview a Beamer slide and shift focus after preview"
  (interactive)
  (setq w1 (selected-window))
  (latex-math-preview-beamer-frame)
  (select-window w1)
  )
(define-key LaTeX-mode-map (kbd "C-c s") 'my-latex-beamer-preview)
;;
;;
;; bibretrieve.el
;; https://github.com/pzorin/bibretrieve; reftex 4.0 not found??
;; (require 'reftex)
;; (require 'bibretrieve)
;;
;; zotelo (Zotero-Local)
;; https://github.com/vitoshka/zotelo for more
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;;
;;
;; YaTeX
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?YaTeX




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


;;; tempbuf.el	Auto-delete unused idle buffers such as dired
;; http://www.emacswiki.org/emacs/tempbuf.el
(require 'tempbuf)
;; No message
(setq tempbuf-kill-message nil)
;; (add-hook 'find-file-hooks		'turn-on-tempbuf-mode)	; All idle unedited files closed
(add-hook 'help-mode-hook		'turn-on-tempbuf-mode)	; Idle help closed
(add-hook 'dired-mode-hook		'turn-on-tempbuf-mode)	; Idle dired closed
(add-hook 'ess-help-mode-hook		'turn-on-tempbuf-mode)	; Idle ESS help closed
(add-hook 'completion-list-mode-hook	'turn-on-tempbuf-mode)	; Idle completion closed
(add-hook 'vc-annotate-mode-hook	'turn-on-tempbuf-mode)	; Idle VC annotate closed
(add-hook 'log-view-mode-hook		'turn-on-tempbuf-mode)	; Idle VC change log closed
(add-hook 'diff-mode-hook		'turn-on-tempbuf-mode)	; Idle VC diff closed
(add-hook 'Snippet-mode-hook		'turn-on-tempbuf-mode)  ; Idle Snippets closed
(add-hook 'Custom-mode-hook		'turn-on-tempbuf-mode)	; Idle M-x customize closed
(add-hook 'fundamental-mode-hook 'turn-on-tempbuf-mode)	; Idle auto-install closed. Not working
(add-hook 'comint-mode-hook		'turn-on-tempbuf-mode)  ; 2013-09-09 for LaTeX inf. process
(add-hook 'latex-math-preview-expression-mode-hook 'turn-on-tempbuf-mode) ; 2013-09-09


;;; Recent files extended
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
;;
;; ;; Supress annoying message in mini-buffer	; 2013-09-27 Crashes by variable depth problems??
;; ;; http://masutaka.net/chalow/2011-10-30-2.html
;; ;; https://gist.github.com/1325654/955277113028eb7b968453a5b7802b74b51b393d
;; (defvar my-recentf-list-prev nil)
;; ;;
;; (defun my-recentf-save-list ()
;;   "If recentf-list and previous recentf-list is equal,
;; do nothing"
;;   (unless (equal recentf-list my-recentf-list-prev)
;;     (recentf-save-list)
;;     (setq my-recentf-list-prev recentf-list)))
;; ;;
;; (defadvice write-region
;;   (around recentf-no-message)
;;   (ad-set-arg 4 'nomsg)
;;   ad-do-it
;;   (set-buffer-modified-p nil))
;; ;;
;; (defadvice recentf-save-list
;;   (around no-message activate)
;;   "suppress the output from message() and write-region() to
;; minibuffer"
;;   (let ((activated (ad-is-active 'write-region)))
;;     (ad-enable-advice 'write-region 'around 'recentf-no-message)
;;     (ad-activate 'write-region)
;;     (unwind-protect
;; 	(flet ((message (format-string &rest args)
;; 			(eval `(format ,format-string ,@args))))
;; 	  ad-do-it)
;;       (ad-disable-advice 'write-region 'around 'recentf-no-message)
;;       (if activated
;; 	  (ad-activate 'write-region)
;; 	(ad-deactivate 'write-region)))))
;; ;;
;; (defadvice recentf-cleanup
;;   (around no-message activate)
;;   "suppress the output from message() to minibuffer"
;;   (flet ((message (format-string &rest args)
;; 		  (eval `(format ,format-string ,@args))))
;;     ad-do-it))
;; ;;
;; ;; (setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
;; ;; (setq recentf-max-saved-items 2000)
;; ;; (setq recentf-exclude '(".recentf"))
;; ;; (setq recentf-auto-cleanup 10)
;; ;; (run-with-idle-timer 30 t 'my-recentf-save-list)
;; ;; (recentf-mode 1)



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


;;; Version control with magit.el
;; Magit User Manual: http://magit.github.io/magit/magit.html
;; emacs wiki magit: http://www.emacswiki.org/emacs/Magit
;; emacs wiki git: http://www.emacswiki.org/emacs/Git
;; http://gom.hatenablog.com/entry/20090524/1243170341
;; Meet Magit on Vimeo: http://vimeo.com/2871241
;; git real basics: http://xahlee.info/linux/git.html
;; magit tutorial: http://ergoemacs.org/emacs/emacs_magit-mode_tutorial.html
(require 'magit)


;;; Auto-intall	; This is causing problem !!! 2013-03-04
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


;;; Helm (Anything successor)
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
;; Disabled because it was giving an error 2013-11-22
(setq helm-locate-command "")
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
;; helm-R.el
(require 'helm-R)
;;
;; helm-migemo.el
(when (eq system-type 'darwin)
    ;; Mac-only
    ;; http://sleepboy-zzz.blogspot.com/2012/09/anythinghelm.html
    (require 'helm-migemo)	
  )
;;
;; wgrep-helm.el  2014-01-14
;; Writable helm-grep-mode buffer and apply the changes to files
(require 'wgrep-helm)



;;; Key-Chord 2013-10-15 disabled to check for speed problem
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
;; (key-chord-define-global "df" 'ess-complete-object-name)		; C-c TAB emulation
;; (key-chord-define-global "sd" 'comint-dynamic-complete-filename)	; M-TAB emulation
;;
;; auto-complete.el
;; (key-chord-define-global "ac" 'auto-complete-mode)	; Toggling auto-complete-mode
;;
;; eval-region
;; (key-chord-define-global "er" 'eval-region)		; Evaluate region for elisp. Not useful.
;;
;; count-words
;; (key-chord-define-global "wc" 'count-words)		; Choose region and activate for word count
;;
;; Comment or Uncomment region
;; (key-chord-define-global "cr" 'comment-region)
;; (key-chord-define-global "ur" 'uncomment-region)	; Dangerous if ur only
;;
;; replace-string
;; (key-chord-define-global "rs" 'replace-string)
;;
;; view-mode
;; (key-chord-define-global "jk" 'view-mode)		; Enter read-only mode
(key-chord-define-global "jk" 'read-only-mode)		; Enter read-only mode
;;
;; crosshairs.el
;; (key-chord-define-global "ch" 'crosshairs-mode)		; Activate continuous crosshairing
;;
;; paredit.el
;; (key-chord-define-global "pm" 'paredit-mode)		; Toggle paredit-mode
;;
;; flyspell-mode
;; (key-chord-define-global "fs" 'flyspell-mode)		; Toggle flyspell-mode
;;
;; Align regexp
;; (key-chord-define-global "nr" 'align-regexp)		; M-x align-regexp.
;;
;; Show in finder
;; (key-chord-define-global "zx" 'show-in-finder)		; M-x show-in-finder
;;
;; bm.el for bookmarking by highlighting
;; (key-chord-define-global "bm" 'bm-toggle)		; ESC-SPC is good enough
;;
;; show-paren-mode
;; (key-chord-define-global "pm" 'show-paren-mode)		; (show-paren-mode)
;;
;; highlight-sexp-mode.el
(key-chord-define-global "sx" 'highlight-sexp-mode)		; (highlight-sexp-mode)
;;
;; ;; tempbuf.el					; does not work
;; (key-chord-define-global "tb" 'tempbuf-mode)		; (tempbuf-mode)



;;; one-key.el  (auto-install as of 2013-08-21)
;; one-key.el via el-get requires hexrgb.el via elpa and does not work (void symbol)
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


;;; view-mode			; C-x C-r to open in view-mode. Requires viewer.el
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
(define-key view-mode-map (kbd "i") 'read-only-mode)
;; Space bar use
(define-key view-mode-map (kbd " ") 'scroll-up)
(define-key view-mode-map (kbd "S-SPC") 'scroll-down)
;; for bm.el
;; (define-key view-mode-map (kbd ".") 'bm-toggle)
;; (define-key view-mode-map (kbd "[") 'bm-previous)
;; (define-key view-mode-map (kbd "]") 'bm-next)
;; (define-key view-mode-map (kbd "[") 'my-bm-previous)
;; (define-key view-mode-map (kbd "]") 'my-bm-next)
;; for highlight-symbol.el
(define-key view-mode-map (kbd ".") 'highlight-symbol-at-point)
(define-key view-mode-map (kbd "[") 'my-highlight-symbol-prev)
(define-key view-mode-map (kbd "]") 'my-highlight-symbol-next)
;;
;; Open non-writable files in view-mode
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))
;; Do not exit view-mode if non-writable
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))
;; Add advice
(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)
;;
;; Activate view-mode for these major modes by activating read-only-mode
;; (add-hook 'emacs-lisp-mode-hook 'read-only-mode)	; This breaks start up screen
;; 
;; To protect R SAS codes. Incompatible with .Rnw
;; http://emacs.1067599.n5.nabble.com/Strange-things-happening-with-rnw-files-in-AucTeX-mode-td288544.html
;; (add-hook 'ess-mode-hook 'read-only-mode)	        
;;
;; Key-bind used instead of "jk"
(global-set-key (kbd "C-c l") 'read-only-mode)



;;; Highlighting
;;
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
;; (setq my-highlight-color "light goldenrod yellow")
(setq my-highlight-color "dark red")
(set-face-background 'hl-line	    my-highlight-color) ; Line color
(set-face-background 'col-highlight my-highlight-color)	; Column color
;; (set-face-background 'hl-line	    "light goldenrod yellow")   ; Line color
;; (set-face-background 'col-highlight "light goldenrod yellow")	; Column color
;; (set-face-background 'hl-line "lemon chiffon")		; Line color
;; (set-face-background 'col-highlight "lemon chiffon")		; Column color


;; auto-highlight-symbol.el for highlighting multiple occurences
;; https://github.com/emacsmirror/auto-highlight-symbol
(require 'auto-highlight-symbol)			; Not good with highlight-symbol.el
;; (global-auto-highlight-symbol-mode t)


;; highligh-symbol for highlighting multiple occurences
;; http://nschum.de/src/emacs/highlight-symbol/
;; http://stackoverflow.com/questions/385661/emacs-highlight-all-occurences-of-a-word
(require 'highlight-symbol)
(global-set-key (kbd "C-.") 'highlight-symbol-at-point)
;; (global-set-key (kbd "C-\}") 'highlight-symbol-next)
;; (global-set-key (kbd "C-\{") 'highlight-symbol-prev)
;;
;; Define highlight-symbol-prev/next and recenter
(defun my-highlight-symbol-prev ()
  (interactive)
  (highlight-symbol-prev)
  (recenter))
(defun my-highlight-symbol-next ()
  (interactive)
  (highlight-symbol-next)
  (recenter))
;;
(global-set-key (kbd "C-\}") 'my-highlight-symbol-next)
(global-set-key (kbd "C-\{") 'my-highlight-symbol-prev)


;; multiple-cursors for simultaneous editing multiple occurrences
;; http://ongaeshi.hatenablog.com/entry/20121205/1354672102 (for a similar package)
;; http://emacsrocks.com/e13.html (video)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
;;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; moccur-edit.el (el-get)
;; Requires color-moccur.el (elpa)
;; http://www.bookshelf.jp/elc/moccur-edit.el
;; http://d.hatena.ne.jp/higepon/20061226/1167098839
;; http://d.hatena.ne.jp/sandai/20120304/p2
(require 'moccur-edit)
;; Modified buffers are saved automatically.
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))
;; Usage:
;; M-x moccur-grep-find to enter Moccur-grep, then objectName .R$
;; r to enter Moccur-edit. C-x C-s to save, C-c C-k


;; ag.el and wgrep-ag.el. Faster replacement for moccur-edit.el 2014-01-14
;; http://yukihr.github.io/blog/2013/12/18/emacs-ag-wgrep-for-code-grep-search/
;; ag.el
(require 'ag)
(setq ag-arguments '("--smart-case" "--group" "--column" "--"))	; grouping is better.
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(setq ag-reuse-window t)
;;
;; wgrep-ag.el
(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)
;; r in ag result buffer invokes edit mode. C-x C-s to save. C-x C-k to cancel.
(define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)


;; highlight-sexp.el
;; http://www.emacswiki.org/emacs/HighlightSexp
;; Color M-x list-colors-display  to check good colors
(require 'highlight-sexp)
;; (setq hl-sexp-background-color "thistle1")
;; (setq hl-sexp-background-color "snow1")
;; (setq hl-sexp-background-color "CadetBlue1") ; for light background
(setq hl-sexp-background-color "dark red") ; for dark background
;; (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
;; (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
;; (add-hook 'ess-mode-hook 'highlight-sexp-mode)	; Not turned on by default use sx to toggle


;; expand-region.el
;; https://github.com/magnars/expand-region.el
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)



;;; Undo tree
;; http://www.emacswiki.org/emacs/UndoTree
;; C-/ for undo. C-? (C-S-/) for redo.
(require 'undo-tree)
(global-undo-tree-mode)


;;; Cursor move undo
;;
;; point-undo.el
;; http://d.hatena.ne.jp/rubikitch/20081230/pointundo
(require 'point-undo)
;; (define-key global-map (kbd "<f5>") 'point-undo)
;; (define-key global-map (kbd "S-<f5>") 'point-redo)
(define-key global-map (kbd "<C-f5>")   'point-undo)
(define-key global-map (kbd "C-S-<f5>") 'point-redo)
;;
;; goto-chg.el
;; http://www.emacswiki.org/emacs/GotoChg
(require 'goto-chg)
;; (global-set-key (kbd "<C-f5>") 'goto-last-change)
;; (global-set-key (kbd "C-S-<f5>") 'goto-last-change-reverse)
(global-set-key (kbd "<f5>")   'goto-last-change)
(global-set-key (kbd "S-<f5>") 'goto-last-change-reverse)


;;; Sequential keys

;; sequential-command.el
;; Book by rubikitch p76. M-x auto-install-batch sequential-command (two files, one -config)
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(require 'sequential-command-config)	; Define these seq-* commands. Needs sequential-command.el
(sequential-command-setup-keys)		; Rebind C-a, C-e, M-u, M-c, and M-l to seq-* commands.
(global-set-key (kbd "C-a") 'seq-home)
(global-set-key (kbd "C-e") 'seq-end)
(global-set-key (kbd "M-u") 'seq-upcase-backward-word)
(global-set-key (kbd "M-c") 'seq-capitalize-backward-word)
(global-set-key (kbd "M-l") 'seq-downcase-backward-word)


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
(add-hook 'inferior-ess-mode-hook 'my-ess-smartchr-setting)
;
(defun my-python-smartchr-setting ()
  (local-set-key (kbd "=") (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+") (smartchr '(" + " "+")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------"))) ; test
  (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################"))) ; test
  ) 
(add-hook 'ein:notebook-multilang-mode-hook	'my-python-smartchr-setting)
(add-hook 'python-mode-hook			'my-python-smartchr-setting)
(add-hook 'inferior-python-mode-hook		'my-python-smartchr-setting)
;;
(defun my-LaTeX-smartchr-setting ()
  (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
  (local-set-key (kbd "%") (smartchr '("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")))
  ) 
(add-hook 'LaTeX-mode-hook 'my-LaTeX-smartchr-setting)
;;
(defun my-elisp-smartchr-setting ()
  (local-set-key (kbd ";") (smartchr '("; " ";; " ";;; " ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))
  ) 
(add-hook 'emacs-lisp-mode-hook 'my-elisp-smartchr-setting)
;;
;; (global-set-key (kbd "=") (smartchr '(" = " " == " "=")))	; =
;; (global-set-key (kbd "(") (smartchr '("(`!!')" "(")))		; ()
;; (global-set-key (kbd "[") (smartchr '("[`!!']" "[")))		; []
;; (global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))		; {}
;; (global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))	; ""
;; (global-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'")))	; ''



;;; open-junk-file.el for creation of permanent test files
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)


;; lispxmp.el to evaluate sexp within .el
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)


;;; paredit.el
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
;; paredit-menu.el
;; Adds a menu to paredit.el as memory aid
(require 'paredit-menu)


;; ;; rainbow-delimiters.el
;; ;; http://www.emacswiki.org/emacs/RainbowDelimiters
;; (require 'rainbow-delimiters)
;; (add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)	; for programming related modes
;; ;;
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(completions-common-part ((t (:inherit default :foreground "red"))))
;;  '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
;;  '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))))
;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
;;  '(show-paren-match ((((class color) (background light)) (:background "azure2")))))



;;; Auto byte-compile .el files at saving
;; http://www.emacswiki.org/emacs/auto-async-byte-compile.el
;; http://d.hatena.ne.jp/rubikitch/20100423/bytecomp
(require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|init.el")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; A simple Emacs interface for the Mercurial (Hg) Distributed SCM. 2013-09-09
(require 'ahg)


;; Use default eldoc (loaded automatically)
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


;;; SQL support				; external dependency
;; http://www.emacswiki.org/emacs/SqlMode
;; http://www.acsu.buffalo.edu/~taowei/wiki/emacs_sql.html
(setq sql-product "mysql")	; for coloring
(setq sql-mysql-program "/usr/local/bin/mysql")
;;
;; sql-indent via package.el
(eval-after-load "sql"
  '(load-library "sql-indent"))
;;
;;
;; Define the MYSQL start up function
(defun my-sql-start-mysql ()
  (interactive)
  (if (not (member "*SQL*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (sql-mysql)			; Start MySQL
        (set-window-buffer w1 "*SQL*")	; SQL on the left (w1)
        (set-window-buffer w2 w1name)	; script on the right (w2)
	(select-window w2)		; Select script (w2) Added
	)))
;;
;; Define the mysql eval function
(defun my-sql-eval ()
  (interactive)
  (my-sql-start-mysql)
  (if (and transient-mark-mode mark-active)	; Check if selection is available
      (sql-send-region (point) (mark))
    (progn
      ;; (beginning-of-line)
      (sql-send-region (point-at-bol) (point-at-eol))
      (next-line))
    ))
;;
;; SQL mode hook
(add-hook 'sql-mode-hook	   
          '(lambda()
	     (local-set-key (kbd "<S-return>") 'my-sql-eval)
	     (local-set-key (kbd "<C-return>") 'my-sql-eval)
	     ))
;; iSQL mode hook
(add-hook 'sql-interactive-mode-hook	   
          '(lambda()
	     ;; (local-set-key (kbd "<S-return>") 'my-sql-eval)
	     ;; (local-set-key (kbd "<C-return>") 'my-sql-eval)
	     ))



;;; Python support				; external dependency
;;
;; python.el (in use):
;; http://caisah.info/emacs-for-python/
;;
;; python-mode.el (more features, more configurations):
;; http://www.janteichmann.me/projects/emacs_as_python_editor
;;
;;
;; python.el
;; http://superuser.com/questions/345595/python-el-for-long-time-python-mode-el-user
(require 'python)
;; (setq python-shell-interpreter "/usr/local/bin/python")	; symlink to python3 does not work.
;;
;; ipython setting for python.el (not used as of 2013-12-28)
(setq
 ;; python-shell-interpreter "ipython"
 python-shell-interpreter "/usr/local/bin/ipython3"
 ;; python-shell-interpreter "/usr/local/bin/ipython"
 ;; "console --pylab" required for matplotlib? 2013-12-25
 ;; http://stackoverflow.com/questions/17117074/python-shell-in-emacs-freezes-when-using-matplotlib
 ;; python-shell-interpreter-args "console --pylab"
 python-shell-interpreter-args "--pylab"	; console does not work with Python 3.3.3. 2013-12-31
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-Regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;
;; ;; Default shell interaction commands
;; (define-key map "\C-c\C-p" 'run-python)
;; (define-key map "\C-c\C-s" 'python-shell-send-string)
;; (define-key map "\C-c\C-r" 'python-shell-send-region)
;; (define-key map "\C-\M-x" 'python-shell-send-defun)
;; (define-key map "\C-c\C-c" 'python-shell-send-buffer)
;; (define-key map "\C-c\C-l" 'python-shell-send-file)
;; (define-key map "\C-c\C-z" 'python-shell-switch-to-shell)
;; 
;; Redefine python-shell-send-region command to avoid sending blank line to ipython shell 2013-12-22
;; This, however, breaks the debugger. It will show the wrong lines in the beginning of the files. 2013-12-25
;; Python3's traceback is smarter and correctly shows the error with the simplified send-region. 2014-01-02
(defun python-shell-send-region (start end)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (python-shell-send-string
   (buffer-substring start end)
   ;; No need to send blank lines in ipython? 2013-12-22
   ;; (concat
   ;;  (let ((line-num (line-number-at-pos start)))
   ;;    ;; When sending a region, add blank lines for non sent code so
   ;;    ;; backtraces remain correct.
   ;;    (make-string (1- line-num) ?\n))
   ;;  (buffer-substring start end))
   nil t))
;;
;; jedi.el	; Python auto-completion for Emacs
;; http://tkf.github.io/emacs-jedi/
(setq jedi:complete-on-dot t)				; binds . to jedi:dot-complete. Dot alone activates jedi
(add-hook 'python-mode-hook	     'jedi:setup)	; for full setup
(add-hook 'inferior-python-mode-hook 'jedi:setup)	; for full setup
;; (add-hook 'python-mode-hook	     'jedi:ac-setup)	; for completion only. keys are not changed.
;; (add-hook 'inferior-python-mode-hook 'jedi:ac-setup)	; for completion only. keys are not changed.
;;
;; C-c		Prefix Command
;; .		jedi:dot-complete
;; <C-M-tab>	jedi:complete
;; <C-tab>		other-window-or-split
;; C-c ,		jedi:goto-definition-pop-marker
;; C-c .		jedi:goto-definition
;; C-c /		helm-jedi-related-names
;; C-c ?		jedi:show-doc
;;
(add-hook 'jedi-mode-hook
          '(lambda()
	     ;; (local-set-key (kbd "<C-M-tab>") 'jedi:complete)	; Assigned to Python major mode
	     (define-key jedi-mode-map (kbd "<C-tab>") 'other-window-or-split)	; Assigned to Jedi minor mode
	     (define-key jedi-mode-map (kbd "<C-M-tab>") 'jedi:complete)	; Assigned to Jedi minor mode
	     ;; jedi:show-doc
	     (define-key jedi-mode-map (kbd "C-c C-v") 'jedi:show-doc)		; Simulate ESS
	     ))
;;
;; Code to emulate ESS/R behavior 2013-12-22 version
(defun my-python-start ()
  (interactive)
  (if (not (member "*Python*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))	; Split into two windows
	(call-interactively 'run-python)	; Activate Python if not running (runs ipython)
        (set-window-buffer w1 "*Python*")	; Python on the left (w1)
        (set-window-buffer w2 w1name)		; Script on the right (w2)
	(select-window w2)			; Select script (w2) Added
	)))
;; Start python if not started. Send region if selected, line if not selected (whole def if it is def)
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_pythonel/
(defun my-python-eval ()
  (interactive)
  (my-python-start)
  (if (and transient-mark-mode mark-active)			; Check if selection is present
      (python-shell-send-region (point) (mark))			; If selected, send region
    ;; If not selected, do all the following
    (beginning-of-line)						; Move to the beginning of line
    (if (looking-at "def")					; Check if the first word is def (function def)
	(progn							; If it is def
	  (python-shell-send-defun ())				; Send whole def
	  (python-nav-end-of-defun)				; Move to the end of def
	  (python-nav-forward-statement)			; Move to the next statement
	  )
      ;; If it is not def, do all the following
      (python-shell-send-region (point-at-bol) (point-at-eol))	; Send the current line
      (python-nav-forward-statement)				; Move to the next statement
      )
    ;; Activate shell window, and switch back
    (progn
      (setq w-script (selected-window))				; Remeber the script window
      (python-shell-switch-to-shell)				; Switch to the shell
      (select-window w-script)					; Switch back to the script window
      )
    ))
;; 
;; Define hooks 
(add-hook 'python-mode-hook		; For Python script
          '(lambda()
	     (local-set-key (kbd "<S-return>") 'my-python-eval)
	     (local-set-key (kbd "<C-return>") 'my-python-eval)	     
	     (local-unset-key (kbd "DEL"))	; Disable python-indent-dedent-line-backspace
	     ;; (eldoc-mode 1)			; eldoc in the mode line. Slow? 2013-12-25
	     (local-set-key (kbd "C-m") 'newline-and-indent)	; Indent after newline
	     (local-set-key (kbd "M-p") 'ess-nuke-trailing-whitespace)
	     (local-set-key (kbd "C-c c") 'python-check)	; Check grammar
	     ))
;;
(add-hook 'inferior-python-mode-hook	; For Python process
          '(lambda()
             ;; (local-set-key (kbd "C-<up>") 'comint-previous-input)
             ;; (local-set-key (kbd "C-<down>") 'comint-next-input)
	     ))
;;
;;
;; ein.el	; Emacs IPython Notebook (EIN) ; Current version does not work with ipython 2.0.0 as of 2013-12-20
;; This is fundamentally different from python.el and can coexist. 2013-12-22
;; http://tkf.github.com/emacs-ipython-notebook/
;; Usage
;; Start IPython notebook server with $ ipython notebook --pylab inline
;; Hit M-x ein:notebooklist-open to open notebook list.
(require 'ein)
;; Auto complete for ein
(setq ein:use-auto-complete t)
;; Or, to enable "superpack" (a little bit hacky improvements):
;; (setq ein:use-auto-complete-superpack t)
(add-hook 'ein:notebook-multilang-mode-hook	; For EIN
          '(lambda()
             (local-set-key (kbd "<C-return>") 'ein:worksheet-execute-cell)
             (local-set-key (kbd "<S-return>") 'ein:worksheet-execute-cell)
	     (local-set-key (kbd "C-c a")      'ein:worksheet-insert-cell-above)
	     (local-set-key (kbd "C-c b")      'ein:worksheet-insert-cell-below)
	     (local-set-key (kbd "C-c k")      'ein:worksheet-kill-cell)
	     (local-set-key (kbd "C-c w")      'ein:worksheet-copy-cell)
	     (local-set-key (kbd "C-c y")      'ein:worksheet-yank-cell)
	     (local-set-key (kbd "C-c p")      'ein:worksheet-goto-prev-input)
	     (local-set-key (kbd "C-c n")      'ein:worksheet-goto-next-input)
	     ))
;;
;; ;; elpy.el	; python.el replacement. No need for now. 2013-12-22
;; ;; https://github.com/jorgenschaefer/elpy/wiki
;; ;; Need to install elpy/rope/jedi/flake8 via $ sudo pip install
;; ;; $ sudo pip install --upgrade elpy # to upgrade to the latest elpy
;; ;; $ sudo ln -s /usr/local/bin/flake8 /usr/bin/flake8 # To make it visible to emacs. 2013-12-22
;; ;; 
;; ;; (package-initialize)	
;; ;; To use elpy
;; (elpy-enable)
;; ;; To use ipython	; 2013-12-22 broken? ipython not found
;; (elpy-use-ipython)
;; ;; Simplify modeline
;; (elpy-clean-modeline)
;; ;; jedi as completion backend
;; (setq elpy-rpc-backend "jedi")
;; ;;
;; ;; Fix yas-snippet-dirs (elpy breaks configuration)	; Not necessary as of 2013-12-22
;; ;; (setq yas-snippet-dirs
;; ;;       '("~/.emacs.d/snippets"
;; ;; 	"/Users/kazuki/.emacs.d/el-get/yasnippet/snippets"
;; ;; 	))



;; This is placed at the end as it freezes if EmacsWIki is not responding.
(auto-install-update-emacswiki-package-name t) ; THIS FREEZES EMACS IF EMACSWIKI IS DONW!!!!!!
;;; end of configuration
