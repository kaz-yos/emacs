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
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))     ; one line at a time
;;(setq mouse-wheel-progressive-speed nil)               ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                      ; scroll window under mouse
;;(setq scroll-step 1)                                   ; keyboard scroll one line at a time
;;


;;;
;;; Scroll other window with M-up/M-down	; Conflict with paredit. Use C-M-(S)-v
;; (global-set-key [M-up]   'scroll-other-window-down)
;; (global-set-key [M-down] 'scroll-other-window)
(global-set-key (kbd "M-<up>")   'scroll-other-window-down)
(global-set-key (kbd "M-<down>") 'scroll-other-window)
(global-set-key (kbd "C-M-t")    'scroll-other-window-down)
(global-set-key (kbd "C-M-v")    'scroll-other-window)


;; Do not open a new frame opening a file from Finder
;; System-specific configuration
;; http://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp
(when (eq system-type 'darwin)
  ;; Do not open a new frame opening a file from Finder
  ;; http://stackoverflow.com/questions/6068819/alias-to-make-emacs-open-a-file-in-a-new-buffer-not-frame-and-be-activated-com
  (setq ns-pop-up-frames nil))


;;;
;;; windresize for M-x windresize
;; M-x windresize, arrows, C-g for cancel, RET to save
(use-package windresize
  :commands windresize)


;;;
;;; window-number.el for direct movement to windows
(use-package window-number
  :config
  (window-number-mode 1)
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
  (global-set-key (kbd "s-4") (lambda () (interactive) (my-window-number-select 4))))


;;;
;;; ELSCREEN-RELATED
;;
;;; elscreen.el
;; https://github.com/knu/elscreen
;; http://www.emacswiki.org/emacs/EmacsLispScreen
;; http://rubikitch.com/2014/09/05/elscreen/
(use-package elscreen
  :init
  ;; Do not set a prefix (conflict with helm)
  (setq elscreen-prefix-key nil)
  ;;
  :config
  ;; Key configs
  (global-set-key (kbd "A-1") 'elscreen-previous)
  (global-set-key (kbd "A-2") 'elscreen-next)
  ;; Cloning is more useful than fresh creation
  (global-set-key (kbd "A-c") 'elscreen-clone)
  (global-set-key (kbd "A-k") 'elscreen-kill)
  (global-set-key (kbd "A-r") 'elscreen-screen-nickname)
  ;;
  ;; Do not show tabls to save space
  ;; Can use M-x elscreen-toggle-display-tab
  (setq elscreen-display-tab nil)
  ;; No preceding [X] for closing
  (setq elscreen-tab-display-kill-screen nil)
  ;; No [<->] at the beginning
  (setq elscreen-tab-display-control nil)
  ;;
  ;; buffer-dependent naming scheme
  (setq elscreen-buffer-to-nickname-alist
        '(("^dired-mode$" .
           (lambda ()
             (format "Dired(%s)" dired-directory)))
          ("^Info-mode$" .
           (lambda ()
             (format "Info(%s)" (file-name-nondirectory Info-current-file))))
          ("^mew-draft-mode$" .
           (lambda ()
             (format "Mew(%s)" (buffer-name (current-buffer)))))
          ("^mew-" . "Mew")
          ("^irchat-" . "IRChat")
          ("^liece-" . "Liece")
          ("^lookup-" . "Lookup")))
  ;;
  (setq elscreen-mode-to-nickname-alist
        '(("[Ss]hell" . "shell")
          ("compilation" . "compile")
          ("-telnet" . "telnet")
          ("dict" . "OnlineDict")
          ("*WL:Message*" . "Wanderlust")))
  ;;
  (defun elscreen-toggle-display-tab2 (state)
    "Toggle elscreen tab bar"
    (if state
        (setq elscreen-display-tab t)
      (setq elscreen-display-tab nil)))
  ;;
  ;; It has to kick in.
  (elscreen-start))
