;;; Window management

;;;
;;; winner-mode to restore previous window configurations
;; http://www.emacswiki.org/emacs/WinnerMode
;; Default: C-c <left> to undo window rearragement. C-c <right> to redo.
(winner-mode t)
(global-set-key (kbd "M-<left>")	'winner-undo)		; M-<left>  to undo
(global-set-key (kbd "M-<right>")	'winner-redo)		; M-<right> to redo
;; alt biding
(global-set-key (kbd "A-b")	'winner-undo)
(global-set-key (kbd "A-f")	'winner-redo)


;;;
;;; Useful shortcuts
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
;; other-frame with C-x o, instead of other-window
(global-set-key (kbd "C-x o") 'other-frame)

;;;
;;; Scroll window with C-t/C-v
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


;;;
;;; Scroll other window with M-up/M-down	; Conflict with paredit. Use C-M-(S)-v
;; (global-set-key [M-up]		'scroll-other-window-down)
;; (global-set-key [M-down]	'scroll-other-window)
(global-set-key (kbd "M-<up>")		'scroll-other-window-down)
(global-set-key (kbd "M-<down>")	'scroll-other-window)
(global-set-key (kbd "C-M-t")		'scroll-other-window-down)
(global-set-key (kbd "C-M-v")		'scroll-other-window)


;; Do not open a new frame opening a file from Finder
;; System-specific configuration
;; http://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp
(when (eq system-type 'darwin)
  ;; Do not open a new frame opening a file from Finder
  ;; http://stackoverflow.com/questions/6068819/alias-to-make-emacs-open-a-file-in-a-new-buffer-not-frame-and-be-activated-com
  (setq ns-pop-up-frames nil)
  )


;;;
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


;;;
;;; windresize for M-x windresize
;; M-x windresize, arrows, C-g for cancel, RET to save
(require 'windresize)


;;;
;;; window-number.el for direct movement to windows
(require 'window-number)
(window-number-mode 1)
;;
(defun my-window-number-select (number)
  "Selects the nth window."
  (interactive "P")
  (if (integerp number)
      (let ((window (nth (1- number) (window-number-list))))
        (if (and window (or (not (window-minibuffer-p window))
                            (minibuffer-window-active-p window)))
            (select-window window)
          ;; if not found, activate ace-window
          (ace-select-window)))))
;;
(global-set-key (kbd "s-1") (lambda () (interactive) (my-window-number-select 1)))
(global-set-key (kbd "s-2") (lambda () (interactive) (my-window-number-select 2)))
(global-set-key (kbd "s-3") (lambda () (interactive) (my-window-number-select 3)))
(global-set-key (kbd "s-4") (lambda () (interactive) (my-window-number-select 4)))
