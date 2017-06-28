;;; Nice options to have On by default


;;;
;;; Taken from ESS/Emacs distribution
;; Activate mouse scrolling if in a graphical system
(when (display-graphic-p)
  (mouse-wheel-mode t))
;; Syntax highlighting everywhere
(global-font-lock-mode t)
;; When enabled, the region is highlighted whenever the mark is active
(transient-mark-mode t)
;; Typed text replaces the selection if the selection is active
(delete-selection-mode t)


;;;
;;; Use newer of .elc or .el
(setq load-prefer-newer t)


;;;
;;; Safe-guard against quiting.
(global-set-key (kbd "C-x C-c") #'(lambda ()
                                    (interactive)
                                    (message "Use M-x save-buffers-kill-terminal")))


;;;
;;; Fewer garbage collection
;; Number of bytes of consing between garbage collections.
(setq garbage-collection-messages t)
;; Set different gc-cons-threshold values depending on the context.
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun set-gc-cons-threshold-max ()
  "Set gc-cons-threshold to maximum"
  (setq gc-cons-threshold most-positive-fixnum))
(set-gc-cons-threshold-max)
;;;
(defun set-gc-cons-threshold-normal ()
  "Set gc-cons-threshold to some reasonable value"
  (setq gc-cons-threshold 8000000))
(add-hook 'after-init-hook 'set-gc-cons-threshold-normal)
;; (add-hook 'minibuffer-setup-hook #'set-gc-cons-threshold-max)
;; (add-hook 'minibuffer-exit-hook #'set-gc-cons-threshold-normal)


;;;
;;; Visible-bell instead of audible
;; (setq visible-bell t)
;;
;; 2015-10-22 Work around for El Capitan
;; http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
(setq visible-bell nil)
;; (setq ring-bell-function 'ignore)
(setq ring-bell-function '(lambda ()
                            (message "Bell!")))


;;;
;;; Never use tabs for indentation
;; Not working?
;; (setq tab-width 4)
(set-default 'indent-tabs-mode nil)
(setq indent-tabs-mode nil)


;;;
;;; C-u C-SPC C-SPC C-SPC ... to follow implicit marks
;; http://rubikitch.com/2016/02/14/sd1506-builtin/
(setq set-mark-command-repeat-pop t)


;;;
;;; Bars: Menu bar only. No scroll bar or tool bar.
;; http://www.emacswiki.org/emacs/FullScreen#toc7
(menu-bar-mode t)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))


;;;
;;; Show the full path in the frame bar (title bar)
;; http://stackoverflow.com/questions/8945056/emacs-how-to-show-the-current-directory-in-the-frame-bar
(setq frame-title-format '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))


;;;
;;; Start up related
;;; No start up message
(setq inhibit-startup-message t)
;;
;;; Screen maximization and full-screen
;; http://www.emacswiki.org/emacs/FullScreen
(setq initial-frame-alist
      '((fullscreen . maximized)))


;;;
;;; Faster pop-to-mark
;; http://endlessparentheses.com/faster-pop-to-mark-command.html?source=rss
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;; C-u C-SPC C-SPC...
(setq set-mark-command-repeat-pop t)


;;;
;;; Line number settings
;;
;; Toggle column number display in the mode line (Column Number mode). (row, col)
(column-number-mode 1)
;;
;; delay linum for speed (linum not used
;; http://d.hatena.ne.jp/daimatz/20120215/1329248780
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))


;;;
;;; Suppress all dialog boxes completely. No need for mouse.
;; http://www.gnu.org/s/libtool/manual/emacs/Dialog-Boxes.html
(setq use-dialog-box nil)


;;;
;;; Allow non-default behaviors
;; Upcase/downcase allowed
(put 'upcase-region	'disabled nil)
(put 'downcase-region	'disabled nil)


;;;
;;; ffap find-file-at-point
;; Get file path or URL from text in the current line
;; http://www.gnu.org/s/libtool/manual/emacs/FFAP.html
(ffap-bindings)
;; A wild card in ffap is passed to dired to get a filtered list of files
(setq ffap-pass-wildcards-to-dired t)


;;;
;;; Allow more variables
;; Increase max values for number of variables that can be defined 2013-09-19
;; http://mikio.github.io/article/2012/06/26_variable-binding-depth-exceeds-max-specpdl-size.html
(setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 6000)


;;;
;;; CUA configurations without rectangles
;; Common User Access mode for column editing: Activated by C-RET while selecting text
;; http://tech.kayac.com/archive/emacs-rectangle.html
;; http://trey-jackson.blogspot.com/2008/10/emacs-tip-26-cua-mode-specifically.html
;; http://stackoverflow.com/questions/3750332/how-do-i-force-a-binding-in-emacs
(setq cua-rectangle-mark-key (kbd "<C-S-return>")) ; <C-S-return> for rectangle
(cua-mode t)
(setq cua-enable-cua-keys nil)			; C-x C-c C-v left intact
;; (setq cua-keep-region-after-copy t)		; Keep selection after copying (Mac/Win-like)


;;;
;;; New line at the EOF automatically
;; Whether to add a newline automatically at the end of the file.
;; A value of t means do this only when the file is about to be saved.
(setq require-final-newline t)


;;;
;;; Faster echo in the echo area
;; Show echo like C-x fast 2014-02-20
(setq echo-keystrokes 0.1)


;;;
;;; Use Mac OS X system trash
;; http://www.masteringemacs.org/articles/2010/12/30/making-deleted-files-trash-can/
;; http://www.reddit.com/r/emacs/comments/iuyef/emacs_on_mac/
(when (eq system-type 'darwin)
  ;; Mac-only
  (setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs"))


;;;
;;; Retain history between sessions
;; Book by rubikitch p59
;; http://www.emacswiki.org/emacs/SaveHist
(savehist-mode 1)
;; https://stackoverflow.com/questions/1229142/how-can-i-save-my-mini-buffer-history-in-emacs
(setq savehist-file (concat user-emacs-directory
                            "history"
                            "_"
                            system-name))


;;;
;;; Remeber the cursor position in a file
(use-package saveplace
  :config
  (setq save-place-file (concat user-emacs-directory
                                "emacs-places"
                                "_"
                                system-name))
  ;; New in 25.1
  (save-place-mode t))


;;;
;;; y or n abbreviations for yes or no
(defalias 'yes-or-no-p 'y-or-n-p)


;;;
;;; C-k (kill-line) with no arg at start of line kills the whole line.
;; vi dd-like behavior
(setq kill-whole-line t)


;;;
;;; Regular expression fontlock
;; rubikitch elisp book page 71
(set-face-foreground 'font-lock-regexp-grouping-backslash "green3")
(set-face-foreground 'font-lock-regexp-grouping-construct "green")


;;;
;;; No emacs-default autosave or backup
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html
(setq auto-save-default nil)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html
(setq make-backup-files nil)


;;;
;;; Killing buffers and frames
;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "A-k") 'kill-this-buffer)
;; http://pragmaticemacs.com/emacs/a-better-shortcut-for-delete-frame/
(global-set-key (kbd "C-x w") 'delete-frame)
;; Prevent killing emacs accidentaly
(global-unset-key (kbd "s-q"))


;;;
;;; Add the system clipboard to the emacs kill-ring
;; http://pragmaticemacs.com/emacs/add-the-system-clipboard-to-the-emacs-kill-ring/
(setq save-interprogram-paste-before-kill t)
