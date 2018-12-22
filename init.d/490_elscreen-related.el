;;; 490_elscreen-related.el ---                      -*- lexical-binding: t; -*-

;;;
;;; ELSCREEN-RELATED

;; This package has to come before elscreen.
;;;  elscreen-separate-buffer-list.el
;; separate buffer list for each elscreen
;; https://github.com/wamei/elscreen-separate-buffer-list
(use-package elscreen-separate-buffer-list
  ;; This is called just before elscreen activation.
  :commands (elscreen-separate-buffer-list-mode
             ;; Used in my-ivy-switch-buffers
             esbl-get-separate-buffer-list))
;; Less safe alternative
;; https://github.com/jeffgran/elscreen-buffer-group


;;;  elscreen.el
;; https://github.com/knu/elscreen
;; http://www.emacswiki.org/emacs/EmacsLispScreen
;; http://rubikitch.com/2014/09/05/elscreen/
(use-package elscreen
  :commands (elscreen-create
             my-elscreen-setup)
  :hook ((after-init . my-elscreen-setup))
  :bind (:map elscreen-map
              ("o" . my-open-current-buffer-in-new-elscreen))
  ;;
  :config
  (defun my-elscreen-setup ()
    "Set up several screens at startup."
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
    (elscreen-goto 0))
  ;;
  (defun my-open-current-buffer-in-new-elscreen ()
    "Open current buffer as a single window in a new elscreen"
    (interactive)
    (let ((current-buffer-name (buffer-name)))
      (elscreen-create)
      (switch-to-buffer current-buffer-name)))
  ;;
  ;; Prefix condition on environment
  (setq elscreen-prefix-key (if (display-graphic-p)
                                (kbd "C-;")
                              (kbd "C-c m ;")))
  ;; Non-nil to display the number of current screen in the mode line.
  (setq elscreen-display-screen-number nil)
  ;; Do not show tabls to save space
  ;; Can use M-x elscreen-toggle-display-tab
  (setq elscreen-display-tab (not (display-graphic-p)))
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
  ;; Activate elscreen-separate-buffer-list first.
  (elscreen-separate-buffer-list-mode +1)
  ;; Activate elscreen itself.
  (elscreen-start))
