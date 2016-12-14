;;; 500_window-management-packages.el ---            -*- lexical-binding: t; -*-

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
  (setq elscreen-prefix-key (kbd "C-;"))
  ;;
  :config
  ;; Key configs
  ;; (global-set-key (kbd "A-1") 'elscreen-previous)
  ;; (global-set-key (kbd "A-2") 'elscreen-next)
  ;; ;; Cloning is more useful than fresh creation
  ;; (global-set-key (kbd "A-c") 'elscreen-clone)
  ;; (global-set-key (kbd "A-k") 'elscreen-kill)
  ;; (global-set-key (kbd "A-r") 'elscreen-screen-nickname)
  (global-set-key (kbd "C-; t") 'elscreen-toggle-display-tab)
  (global-set-key (kbd "C-; l") 'helm-elscreen)
  (global-set-key (kbd "C-; h") 'helm-elscreen)
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
  ;; Use frame-title for tabs
  ;; How to display the list of screens on the frame-title of my Emacs?
  ;; This is broken. get-alist should be changed to alist-get
  ;; https://www.emacswiki.org/emacs/EmacsLispScreen#toc8
  ;;
  ;; More modern way to use frame title
  ;; Define truncation length
  (setq elscreen-tab-truncate-length 20)
  ;;
  (defun elscreen-tabs-as-string ()
    "Return a string representation of elscreen tab names

Set nae truncation length in ELSCREEN-TRUNCATE-LENGTH"
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (screen-to-name-alist (elscreen-get-screen-to-name-alist)))
      ;; mapconcat: mapping and then concate name elements together with separator
      (mapconcat
       (lambda (screen)
         (format (if (string-equal "+" (elscreen-status-label screen))
                     ;; Current screen format
                     "[ %d ] %s"
                   ;; Others
                   "(%d) %s")
                 ;; screen number: replaces %d (integer)
                 screen
                 ;; screen name: replaces %s (string)
                 (elscreen-truncate-screen-name
                  ;; Return the value associated with KEY in ALIST
                  (alist-get screen screen-to-name-alist)
                  elscreen-tab-truncate-length)))
       ;; Screen numbers (keys for alist)
       screen-list
       ;; Separator
       " | ")))
  ;;
  (defun elscreen-tabs-file-path ()
    "Return a string containing tab names and file path

Concate name elscreen tab names and current buffer name or
associated file name."
    (interactive)
    (let* ((buffer-name-part (if buffer-file-name
                                 (abbreviate-file-name buffer-file-name)
                               (buffer-name)))
           (elscreen-tab-part (elscreen-tabs-as-string)))
      ;; How they are combined together
      (concat elscreen-tab-part "    ||    " buffer-name-part)))
  ;; Using frame-title-format avoids the need for a hook
  (setq frame-title-format '(:eval (elscreen-tabs-file-path)))
  ;;
  ;; It has to kick in.
  (elscreen-start))
