;;; 500_window-management-packages.el ---            -*- lexical-binding: t; -*-

;;;
;;; sticky-windows.el
(use-package sticky-windows
  :commands (sticky-window-keep-window-visible
             sticky-window-delete-window
             sticky-window-delete-other-windows))


;;;
;;; windresize for M-x windresize
;; M-x windresize, arrows, C-g for cancel, RET to save
(use-package windresize
  :commands windresize)


;;;
;;; window-number.el for direct movement to windows
(use-package window-number
  :config
  ;; No need to show numbers in the mode-line.
  ;; (window-number-mode 1)
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
  (global-set-key (kbd "s-4") (lambda () (interactive) (my-window-number-select 4))))


;;;
;;; zoom-window.el
;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :commands (zoom-window-zoom)
  :config
  (setq zoom-window-use-elscreen t)
  (zoom-window-setup))


;;;
;;; ELSCREEN-RELATED
;;
;;;  elscreen.el
;; https://github.com/knu/elscreen
;; http://www.emacswiki.org/emacs/EmacsLispScreen
;; http://rubikitch.com/2014/09/05/elscreen/
(use-package elscreen
  :commands (elscreen-toggle-display-tab)
  :init
  ;; Condition on environment
  (setq elscreen-prefix-key (if (display-graphic-p)
                                (kbd "C-;")
                              (kbd "C-c m ;")))
  ;; Set up screens nicely
  (add-hook 'after-init-hook
            (lambda ()
              ;; Screen 1
              (progn (elscreen-create)
                     (elscreen-screen-nickname "init.d")
                     (find-file (concat user-emacs-directory "init.d"))
                     (split-window nil nil 'left))
              ;; Screen 2
              (progn (elscreen-create)
                     (find-file (if (file-exists-p "~/Documents")
                                    "~/Documents"
                                  "~"))
                     (split-window nil nil 'left))
              ;; Screen 0
              (elscreen-goto 0)))
  ;;
  :config
  ;; Non-nil to display the number of current screen in the mode line.
  (setq elscreen-display-screen-number nil)
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
;;;  Use frame-title for tabs
  ;; How to display the list of screens on the frame-title of my Emacs?
  ;; This is broken. get-alist should be changed to alist-get
  ;; https://www.emacswiki.org/emacs/EmacsLispScreen#toc8
  ;; elscreen-fr.el is a package that does a similar thing.
  ;; https://github.com/rocher/elscreen-fr
  ;;
  (defvar *elscreen-tab-truncate-length*
    20
    "Number of characters to truncate tab names in frame title")
  ;;
  (defun elscreen-tabs-as-string ()
    "Return a string representation of elscreen tab names

Set name truncation length in ELSCREEN-TRUNCATE-LENGTH"
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (screen-to-name-alist (elscreen-get-screen-to-name-alist)))
      ;; mapconcat: mapping and then concate name elements together with separator
      (mapconcat
       (lambda (screen)
         (format (if (string-equal "+" (elscreen-status-label screen))
                     ;; Current screen format
                     "<< [%d] %s >>"
                   ;; Others
                   "(%d) %s")
                 ;; screen number: replaces %d (integer)
                 screen
                 ;; screen name: replaces %s (string)
                 (elscreen-truncate-screen-name
                  ;; Return the value associated with KEY in ALIST
                  (alist-get screen screen-to-name-alist)
                  *elscreen-tab-truncate-length*)))
       ;; Screen numbers (keys for alist)
       screen-list
       ;; Separator
       "    ")))
  ;;
  (defvar *elscreen-tabs-as-string*
    ""
    "Variable to hold curent elscreen tab names as a string")
  ;;
  (defun update-elscreen-tabs-as-string ()
    "Update *elscreen-tabs-as-string* variable"
    (interactive)
    (setq *elscreen-tabs-as-string* (elscreen-tabs-as-string)))
  ;;
  ;; Update *elscreen-tabs-as-string* whenever elscreen status updates
  (add-hook 'elscreen-screen-update-hook 'update-elscreen-tabs-as-string)
  ;;
  ;; Set frame title format as combination of current elscreen tabs and buffer/path
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Properties.html
  ;; http://kitchingroup.cheme.cmu.edu/blog/2014/09/14/Colorized-text-in-Emacs/
  (setq frame-title-format '(:eval (propertize
                                    ;; String
                                    (concat *elscreen-tabs-as-string*
                                            "    ||    "
                                            (if buffer-file-name
                                                (abbreviate-file-name buffer-file-name)
                                              "%b"))
                                    ;; Properties: a sequence of PROPERTY VALUE pairs for text
                                    'face '(:foreground "white"))))
  ;;
  ;; It has to kick in.
  (elscreen-start))


;;;
;;; window-purpose.el
;; https://github.com/bmag/emacs-purpose
(use-package window-purpose
  :commands (purpose-mode)
  :config
  ;; Purpose Configuration:
  ;; Customize `purpose-user-mode-purposes', `purpose-user-name-purposes',
  ;; `purpose-user-regexp-purposes' and
  ;; `purpose-use-default-configuration'.
  ;;
  ;; Basic Usage:
  ;; 1. Load/Save window/frame layout (see `purpose-load-window-layout',
  ;;    `purpose-save-window-layout', etc.)
  ;; 2. Use regular switch-buffer functions - they will not mess your
  ;;    window layout (Purpose overrides them).
  ;; 3. If you don't want a window's purpose/buffer to change, dedicate
  ;;    the window:
  ;;    C-c , d: `purpose-toggle-window-purpose-dedicated'
  ;;    C-c , D: `purpose-toggle-window-buffer-dedicated'
  ;; 4. To use a switch-buffer function that ignores Purpose, prefix it
  ;;    with C-u. For example, [C-u C-x b] calls
  ;;    `switch-buffer-without-purpose'.
  ;;
  ;; Activate
  (purpose-mode -1))
