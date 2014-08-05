;;; Nice options to have On by default
;; Activate mouse scrolling
(mouse-wheel-mode t)
;; Syntax highlighting everywhere
(global-font-lock-mode t)
;; When enabled, the region is highlighted whenever the mark is active
(transient-mark-mode t)
;; Typed text replaces the selection if the selection is active
(delete-selection-mode t)


;;;
;;; Visible-bell instead of audible
(setq visible-bell t)


;;;
;;; Never use tabs for indentation
;; Not working?
;; (setq tab-width 4)
(setq indent-tabs-mode nil)


;;;
;;; Bars: Menu bar only. No scroll bar or tool bar.
;; http://www.emacswiki.org/emacs/FullScreen#toc7
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)


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


;;;
;;; Remeber the cursor position in a file
;; http://www.emacswiki.org/emacs/SavePlace
;; http://git.sysphere.org/dotfiles/tree/emacs
(setq save-place-file "~/.emacs.d/emacs-places")		; save file within ~/.emacs.d
(setq-default save-place t)
(require 'saveplace)


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
