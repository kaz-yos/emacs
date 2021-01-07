;;; e-mail related                                 -*- lexical-binding: t; -*-
;; This file is public on github. Do not put sensitive information here.


;;;
;;; Sending-related
;; These are designed to utilize the `msmtp' SMTP client.
;; In its default mode of operation, `msmtp' reads a mail from standard input
;; and sends it to a predefined SMTP server that takes care of proper delivery.
;; Command line options and exit codes are compatible to sendmail.
;;
;; https://marlam.de/msmtp/
;; https://marlam.de/msmtp/documentation/
;; https://marlam.de/msmtp/msmtp.html#Examples
;; https://github.com/marlam/msmtp-mirror
;;
;; Emacs user configuration example
;; https://www.emacswiki.org/emacs/GnusMSMTP
;; https://tushartyagi.com/blog/configure-mu4e-and-msmtp/
;; https://www.ict4g.net/adolfo/notes/emacs/reading-imap-mail-with-emacs.html
;;
;; The following variables were configured based on the URL.
;; `mail-specify-envelope-from' `mail-envelope-from' `message-sendmail-envelope-from'
;; https://jonathanchu.is/posts/emacs-notmuch-isync-msmtp-setup/
;;
;;;  sendmail.el
;; https://www.gnu.org/software/emacs/manual/html_mono/smtpmail.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mail-Sending.html
(use-package sendmail
  :config
  ;; Program used to send messages. Use `msmtp' instead of sendmail.
  ;; $ brew install mstmp
  (setq sendmail-program (executable-find "msmtp"))
  ;; Function to call to send the current buffer as mail.
  ;; The headers should be delimited by a line which is
  ;; not a valid RFC 822 (or later) header or continuation line,
  ;; that matches the variable mail-header-separator.
  ;; This is used by the default mail-sending commands.  See also
  ;; `message-send-mail-function' for use with the Message package.
  ;; `sendmail-send-it'
  ;; Send the current mail buffer using the Sendmail package.
  ;; This is a suitable value for `send-mail-function'.  It sends using the
  ;; external program defined by `sendmail-program'.
  (setq send-mail-function 'sendmail-send-it)
  ;; If non-nil, specify the envelope-from address when sending mail.
  ;; The value used to specify it is whatever is found in
  ;; the variable `mail-envelope-from', with user-mail-address as fallback.
  ;; On most systems, specifying the envelope-from address is a
  ;; privileged operation.  This variable affects sendmail and
  ;; smtpmail -- if you use feedmail to send mail, see instead the
  ;; variable feedmail-deduce-envelope-from.
  (setq mail-specify-envelope-from t)
  ;; If non-nil, designate the envelope-from address when sending mail.
  ;; This only has an effect if mail-specify-envelope-from is non-nil.
  ;; The value should be either a string, or the symbol header (in
  ;; which case the contents of the "From" header of the message
  ;; being sent is used), or nil (in which case the value of
  ;; user-mail-address is used).
  (setq mail-envelope-from 'header))
;;
;;;  message.el
(use-package message
  :config
  ;; `message-send-mail-function'
  ;; Function to call to send the current buffer as mail.
  ;; The headers should be delimited by a line whose contents match the
  ;; variable mail-header-separator.
  ;; Valid values include message-send-mail-with-sendmail
  ;; message-send-mail-with-mh, message-send-mail-with-qmail,
  ;; message-smtpmail-send-it, smtpmail-send-it,
  ;; feedmail-send-it and message-send-mail-with-mailclient.  The
  ;; default is system dependent and determined by the function
  ;; message-send-mail-function.
  ;; See also send-mail-function.
  ;; This variable was added, or its default value changed, in Emacs 27.1.
  ;; `message-send-mail-with-sendmail'
  ;; Send off the prepared buffer with sendmail.
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;; Additional arguments to sendmail-program.
  ;; Tell `msmtp' to choose the SMTP server according to the from field
  ;; in the outgoing email
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  ;; Non-nil means don't add "-f username" to the sendmail command line.
  ;; Required for proper msmtp functioning.
  (setq message-sendmail-f-is-evil 't)
  ;; Envelope-from when sending mail with sendmail.
  ;; If this is nil, use user-mail-address.  If it is the symbol
  ;; header, use the From: header of the message.
  (setq message-sendmail-envelope-from 'header)
  ;;
  ;; Drop the date header before sending.
  ;; This avoids the message's date remaining the first time the draft
  ;; message was made.
  (defun my-mail-date ()
    "Move point to end of Date field, creating it if necessary."
    (interactive)
    (expand-abbrev)
    (mail-position-on-field "Date"))
  ;;
  (defun my-mail-drop-date ()
    "Drop Date field."
    (interactive)
    ;; Move to the end of Date field
    (my-mail-date)
    ;; Delete whole line
    ;; https://www.emacswiki.org/emacs/ElispCookbook#toc16
    (let ((beg
           ;; Obtain the point of the beginning of the CURRENT line.
           (progn (forward-line 0) (point)))
          (end
           ;; Obtain the point of the beginning of the NEXT line.
           (progn (forward-line 1)
                  (point))))
      ;; This deletes the entire line including the newline character.
      (delete-region beg end)))
  ;;
  ;; Drop date field right before sending.
  (advice-add 'message-send-and-exit
              :before
              #'my-mail-drop-date)
  ;;
  ;; When non-nil, ask for confirmation when sending a message.
  (setq message-confirm-send t))


;;;
;;; mu4e-related
;; http://www.djcbsoftware.nl/code/mu/
;; http://www.djcbsoftware.nl/code/mu/mu4e.html
;; https://www.emacswiki.org/emacs/mu4e
;;
;; Manage your gmail account in emacs with mu4e
;; https://gist.github.com/areina/3879626
;; Installing mu and mu4e with homebrew with emacs from emacsforosx
;; http://blog.danielgempesaw.com/post/43467552978/installing-mu-and-mu4e-with-homebrew-with-emacs
;; MU4E Tutorials
;; http://pragmaticemacs.com/mu4e-tutorials/
;; Irreal on mu4e
;; https://irreal.org/blog/?tag=mu4e
;; The Ultimate Emailing Agent with Mu4e and Emacs
;; http://tech.memoryimprintstudio.com/the-ultimate-emailing-agent-with-mu4e-and-emacs/
;; Fastmail setup with Emacs, mu4e and mbsync on macOS
;; https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
;;
;; Install infrastructure
;; Install HEAD mu along with mu4e following below
;; Make sure $ which emacs gives the path to a new emacs (23+)
;; https://github.com/Homebrew/homebrew/issues/16504#issuecomment-11394215
;; As of 2020-12-18, use the following:
;; EMACS=$(which emacs) brew install mu
;;
;; These unix tools also have to be installed and configured elsewhere.
;; `mbsync' (isync package) for IMAP-Maildir syncing
;; `msmtp' for sending messages
;; pass and gpg for encrypted password handling (instead macOS security command in use)
;;
;; Put in the shell environment.
;; # CJK searching in mu4e
;; # http://www.djcbsoftware.nl/code/mu/mu4e/General.html
;; export XAPIAN_CJK_NGRAM="TRUE"
;;
;;
;;; mu4e.el
;; Reading IMAP Mail in Emacs on OSX
;; http://www.ict4g.net/adolfo/notes/2014/12/27/EmacsIMAP.html
(use-package mu4e
  :if (executable-find "mu")
  :init
  ;; Check for mu4e directory before invoking all the following
  (let ((mu4e-dir "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e/"))
    (when (file-exists-p mu4e-dir)
      (add-to-list 'load-path mu4e-dir)))
  ;;
  :commands (mu4e
             mu4e-background
             make-mu4e-context)
  :hook (;; (after-init . mu4e-background)
         (mu4e-headers-mode . my-mu4e-set-header-columns-width))
  ;; C-i appears as TAB
  ;; https://emacs.stackexchange.com/questions/17509/how-to-distinguish-c-i-from-tab/17510
  :bind (:map mu4e-main-mode-map
         ("C-c C-u" . mu4e-update-mail-and-index)
         ("C-c C-m" . my-mu4e-update-mail-and-index-main)
         ("C-c C-a" . my-mu4e-update-mail-and-index-all)
         ("C-c C-i" . mu4e-update-index)
         ;;
         :map mu4e-headers-mode-map
         ("C-c C-u" . mu4e-update-mail-and-index)
         ("C-c C-m" . my-mu4e-update-mail-and-index-main)
         ("C-c C-a" . my-mu4e-update-mail-and-index-all)
         ("C-c C-i" . mu4e-update-index)
         ;;
         :map mu4e-view-mode-map
         ("C-c C-u" . mu4e-update-mail-and-index)
         ("C-c C-m" . my-mu4e-update-mail-and-index-main)
         ("C-c C-a" . my-mu4e-update-mail-and-index-all)
         ("C-c C-i" . mu4e-update-index)
         ("G" . mu4e-view-go-to-url)
         ("g" . 'mu4e-headers-rerun-search)
         ;;
         :map mu4e-compose-mode-map
         ("C-c C-u" . mu4e-update-mail-and-index)
         ("C-c C-m" . my-mu4e-update-mail-and-index-main)
         ("C-c C-a" . my-mu4e-update-mail-and-index-all)
         ("C-c C-i" . mu4e-update-index)
         ;;
         :map my-key-map
         (";" . mu4e))
  ;;
  :config
  (defun mu4e-background () (mu4e t))
  ;;
  ;; mu binary (backend)
  (setq mu4e-mu-binary (executable-find "mu"))
  ;;
  ;;
;;;  Syncing
  ;; Shell command to run to retrieve new mail.
  ;; Defined by defcustom, thus, a dynamic variable.
  ;; inbox-only is a group of inbox channels and does not sync other boxes
  (if (executable-find "parallel")
      (setq mu4e-get-mail-command "parallel mbsync -V {1}-inbox ::: icloud harvard channing mgb")
    (setq mu4e-get-mail-command "mbsync inbox-only"))
  ;;
  ;; Define a function to list visible buffer names.
  (defun my-visible-buffer-names ()
    "Return a list of visible buffer names"
    (let* ((all-buffer-names (mapcar (function buffer-name) (buffer-list))))
      (seq-filter 'get-buffer-window
                  all-buffer-names)))
  ;; Function to check mu4e visibility.
  (defun my-mu4e-buffer-visible-p ()
    "Return non-nil if any buffer with a name containing mu4e has a window"
    (seq-filter (lambda (str) (string-match "*mu4e" str))
                (my-visible-buffer-names)))
  ;;
  ;; Define a function to change mbsync behavior when called interactively
  (defun my-modify-mu4e-get-mail-command (run-in-background)
    "Manipulate mu4e-get-mail-command depending on interactive status"
    ;; "P" is for universal argument
    ;; http://ergoemacs.org/emacs/elisp_universal_argument.html
    (interactive "P")
    (cond
     ;; If interactive,
     ((called-interactively-p 'interactive)
      ;; Conduct abbreviated operations.
      (setq mu4e-cache-maildir-list t)
      (setq mu4e-index-cleanup nil)
      (setq mu4e-index-lazy-check t)
      ;; Inbox-only
      (if (executable-find "parallel")
          (setq mu4e-get-mail-command "parallel mbsync -V {1}-inbox ::: icloud harvard channing mgb")
        (setq mu4e-get-mail-command "mbsync inbox")))
     ;;
     ;; If any mu4e windows are active, main boxes only.
     ;; This happens even if update is running non-interactively.
     ;; Currently, this is avoided with `my-mu4e-update-mail-and-index-if-mu4e-invisible',
     ;; which is used in the timer.
     ((my-mu4e-buffer-visible-p)
      (setq mu4e-cache-maildir-list t)
      (setq mu4e-index-cleanup nil)
      (setq mu4e-index-lazy-check t)
      ;; Main boxes
      (if (executable-find "parallel")
          (setq mu4e-get-mail-command "parallel mbsync -V {1}-{2} ::: icloud harvard channing mgb ::: inbox sent drafts trash")
        (setq mu4e-get-mail-command "mbsync main")))
     ;;
     ;; Otherwise, conduct an all-boxes, thorough operation.
     (t
      (setq mu4e-cache-maildir-list nil)
      (setq mu4e-index-cleanup t)
      (setq mu4e-index-lazy-check nil)
      ;; All boxes
      (if (executable-find "parallel")
          (setq mu4e-get-mail-command "parallel mbsync -V {1} ::: icloud harvard channing mgb")
        (setq mu4e-get-mail-command "mbsync all")))))
  ;; :before advice to mainpulate mu4e-get-mail-command variable
  (advice-add 'mu4e-update-mail-and-index :before #'my-modify-mu4e-get-mail-command)
  (setq mu4e~update-buffer-height 4)
  ;; (advice-remove 'mu4e-update-mail-and-index #'my-modify-mu4e-get-mail-command)
  ;;
  ;; Function to force-sync inbox folders
  (defun my-mu4e-update-mail-and-index-inbox (run-in-background)
    "Update email with main folder syncing"
    (interactive "P")
    (setq mu4e-cache-maildir-list t)
    (setq mu4e-index-cleanup nil)
    (setq mu4e-index-lazy-check t)
    ;; Main boxes
    (if (executable-find "parallel")
        (setq mu4e-get-mail-command "parallel mbsync -V {1}-inbox ::: icloud harvard channing mgb")
      (setq mu4e-get-mail-command "mbsync inbox"))
    ;; This is the body of mu4e-update-mail-and-index to avoid advice.
    (if (and (buffer-live-p mu4e~update-buffer)
             (process-live-p (get-buffer-process mu4e~update-buffer)))
        (mu4e-message "Update process is already running")
      (progn
        (run-hooks 'mu4e-update-pre-hook)
        (mu4e~update-mail-and-index-real run-in-background))))
  ;;
  ;; Function to force-sync main folders
  (defun my-mu4e-update-mail-and-index-main (run-in-background)
    "Update email with main folder syncing"
    (interactive "P")
    (setq mu4e-cache-maildir-list t)
    (setq mu4e-index-cleanup nil)
    (setq mu4e-index-lazy-check t)
    ;; Main boxes
    (if (executable-find "parallel")
        (setq mu4e-get-mail-command "parallel mbsync -V {1}-{2} ::: icloud harvard channing mgb ::: inbox sent drafts trash")
      (setq mu4e-get-mail-command "mbsync main"))
    ;; This is the body of mu4e-update-mail-and-index to avoid advice.
    (if (and (buffer-live-p mu4e~update-buffer)
             (process-live-p (get-buffer-process mu4e~update-buffer)))
        (mu4e-message "Update process is already running")
      (progn
        (run-hooks 'mu4e-update-pre-hook)
        (mu4e~update-mail-and-index-real run-in-background))))
  ;;
  ;; Function to force-sync all folders
  (defun my-mu4e-update-mail-and-index-all (run-in-background)
    "Update email with more extensive folder syncing"
    (interactive "P")
    (setq mu4e-cache-maildir-list nil)
    (setq mu4e-index-cleanup t)
    (setq mu4e-index-lazy-check nil)
    ;; All boxes
    (if (executable-find "parallel")
        (setq mu4e-get-mail-command "parallel mbsync -V {1} ::: icloud harvard channing mgb")
      (setq mu4e-get-mail-command "mbsync all"))
    ;; This is the body of mu4e-update-mail-and-index to avoid advice.
    (if (and (buffer-live-p mu4e~update-buffer)
             (process-live-p (get-buffer-process mu4e~update-buffer)))
        (mu4e-message "Update process is already running")
      (progn
        (run-hooks 'mu4e-update-pre-hook)
        (mu4e~update-mail-and-index-real run-in-background))))
  ;;
  ;; Change file UID when moving (necessary for mbsync, but not for offlineimap)
  ;; https://groups.google.com/forum/m/#!topic/mu-discuss/8c9LrYYpxjQ
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/General.html
  (setq mu4e-change-filenames-when-moving t)
  ;;
  ;; Do not use mu4e's regular interval update.
  ;; Update interval. If nil, don't update automatically.
  (setq mu4e-update-interval nil)
  ;; Only run `mu4e-update-mail-and-index' if mu4e windows are invisible.
  ;; http://stackoverflow.com/questions/3841459/how-to-periodically-run-a-task-within-emacs
  ;; (run-with-timer SECS REPEAT FUNCTION &rest ARGS)
  ;; SECS initial delay
  ;; REPEAT repletion interval
  (defun my-mu4e-update-mail-and-index-main-if-mu4e-invisible-or-idle ()
    "Run mu4e-update-mail-and-index when appropriate.

Run when mu4e buffers are invisible."
    (when (or (not (my-mu4e-buffer-visible-p))
              ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html
              (current-idle-time))
      (my-mu4e-update-mail-and-index-main t)))
  (run-with-timer (* 5 60) (* 5 60) 'my-mu4e-update-mail-and-index-main-if-mu4e-invisible-or-idle)
  ;;
  (defun my-mu4e-update-index ()
    "Minimally update mu4e index."
    (interactive "P")
    (let ((mu4e-cache-maildir-list t)
          (mu4e-index-cleanup nil)
          (mu4e-index-lazy-check t))
      (mu4e-update-index)))
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html
  (run-with-idle-timer 5 t 'my-mu4e-update-index)
  ;;
  ;; Whether to hide the "Indexing..." and contacts messages.
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/General.html
  (setq mu4e-hide-index-messages nil)
  ;; Whether to cache the list of maildirs
  (setq mu4e-cache-maildir-list t)
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.Html
  ;; Whether to run a cleanup phase after indexing.
  ;; That is, validate that each message in the message store has a
  ;; corresponding message file in the filesystem.
  ;; Having this option as t ensures that no non-existing messages are
  ;; shown but can slow with large message stores on slow file-systems.
  (setq mu4e-index-cleanup t)
  ;; Whether to only use a 'lazy check' during reindexing.
  ;; This influences how we decide whether a message
  ;; needs (re)indexing or not. When this is set to t, mu only uses
  ;; the directory timestamps to decide whether it needs to check the
  ;; messages beneath it, which would miss messages that are modified
  ;; outside mu. On the other hand, it's significantly faster.
  (setq mu4e-index-lazy-check nil)
  ;;
  ;;
;;;  Dynamic folder selection (configured elsewhere)
  ;;
;;;  Main view configuration (configured elsewhere)
  ;;
  ;;
;;;  Header view configuration
  (setq mu4e-headers-fields
        '((:human-date . 10)
          (:flags . 4)
          (:from . 22)
          (:subject)))
  ;; How to show messages / headers.
  (setq mu4e-split-view 'vertical)
  ;; Number of columns to display for the header view when using the
  ;; vertical split-view.
  (setq mu4e-headers-visible-columns 100)
  ;; We want to leave at least 125 for the message window's (window-width).
  ;; PubMed email messages has a width of 125.
  ;; Ratio to the entire frame width [0,1]
  (setq my-mu4e-headers-visible-columns-ratio 0.3)
  (defun my-mu4e-set-header-columns-width ()
    "Make header split frame into half in mu4e split view"
    (interactive)
    (setq mu4e-headers-visible-columns
          ;; Return text area width of FRAME in pixels.
          (thread-first (frame-text-width)
            ;; On a graphical screen, the width is the standard width of the default font.
            ;; For a terminal screen, the value is always 1.
            (/ (frame-char-width))
            (* my-mu4e-headers-visible-columns-ratio)
            (truncate))))
  ;;
  ;; X-Keywords is the gmail compatible tag name.
  (setq mu4e-action-tags-header "X-Keywords")
  ;; The header date format for messages received yesterday and before.
  ;; See `format-time-string' for the format
  (setq mu4e-headers-date-format "%Y-%m-%d")
  (setq mu4e-headers-time-format "%X")
  (setq mu4e-headers-long-date-format "%c")
  ;; Show related messages in addition to search results
  (setq mu4e-headers-include-related nil)
  ;; Maximum number of results to show
  (setq mu4e-headers-results-limit 500)
  ;; Whether to automatically update headers if any indexed changes appear
  (setq mu4e-headers-auto-update t)
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Other-search-functionality.html#Skipping-duplicates
  (setq mu4e-headers-skip-duplicates t)
  ;; 4.4 Sort order and threading
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Sort-order-and-threading.html
  ;; :date, :subject, :size, :prio, :from, :to
  (setq mu4e-headers-sort-field :date)
  ;; Threading off by default. use P to turn on.
  (setq mu4e-headers-show-threads nil)
  (setq mu4e-use-fancy-chars nil)
  ;; ‘apply’ automatically apply the marks before doing anything else
  (setq mu4e-headers-leave-behavior 'apply)
  ;;
  ;; Automatically run the following?
  ;; mu4e-headers-rerun-search
  ;;
  ;; Actions
  (defun my-mu4e-action-narrow-messages-to-unread (&optional msg)
    "Narrow current messages to unread only

The optional and unused msg argument is to fit into mu4e's action framework."
    (interactive)
    (mu4e-headers-search-narrow "flag:unread"))
  ;;
  (setq mu4e-headers-actions
        '(("capture message"    . mu4e-action-capture-message)
          ("show this thread"   . mu4e-action-show-thread)
          ("unread among these" . my-mu4e-action-narrow-messages-to-unread)
          ("find same sender"   . my-mu4e-action-find-messages-from-same-sender)
          ("narrow same sender" . my-mu4e-action-narrow-messages-to-same-sender)
          ("apple mail"         . my-mu4e-action-search-message-in-apple-mail)
          ("tag"                . mu4e-action-retag-message)))
  ;;
  ;;
;;;  Message view configuration
  ;; Whether to automatically display attached images in the message
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-view-date-format "%Y-%m-%d %H:%M:%S")
  ;;
  ;; Displaying rich-text messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
  (setq mu4e-view-prefer-html nil)
  ;; mu4e, stop emails setting background/foreground colours etc
  ;; https://www.reddit.com/r/emacs/comments/9ep5o1/mu4e_stop_emails_setting_backgroundforeground/
  ;; Either a shell command or a function that converts from html to plain text.
  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; shr-color.el Minimum luminance distance between two colors to be considered visible.
  (setq shr-color-visible-luminance-min 60)
  ;; shr-color.el Minimum color distance between two colors to be considered visible.
  (setq shr-color-visible-distance-min 5)
  ;; shr.el If non-nil, respect color specifications in the HTML.
  (setq shr-use-colors nil)
  ;; shr.el `shr-colorize-region'
  (advice-add #'shr-colorize-region
              :around (defun shr-no-colourise-region (&rest ignore)))
  ;;
  ;; pdf view
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html
  ;; https://github.com/djcb/mu/issues/443
  ;; http://stackoverflow.com/questions/24194357/glib-h-file-not-found-on-max-osx-10-9
  (setq mu4e-msg2pdf nil)
  ;; Default directory for saving attachments.
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))
  ;;
  ;; mu4e-views.el
  ;; https://github.com/lordpretzel/mu4e-views
  (use-package mu4e-views
    :ensure t
    :demand t
    ;; Use in GUI only.
    :if (display-graphic-p)
    :bind (:map mu4e-headers-mode-map
           ;; select viewing method
           ("v" . mu4e-views-mu4e-select-view-msg-method)
           ;; from headers window scroll the email view
           ("M-n" . mu4e-views-cursor-msg-view-window-down)
           ;; from headers window scroll the email view
           ("M-p" . mu4e-views-cursor-msg-view-window-up)
           ;; toggle opening messages automatically when moving in the headers view
           ("f" . mu4e-views-toggle-auto-view-selected-message))
    :config
    ;; use ivy for completion
    (setq mu4e-views-completion-method 'ivy)
    ;; make xwidgets default
    (setq mu4e-views-default-view-method "html")
    ;; select the default
    (mu4e-views-mu4e-use-view-msg-method "html")
    ;; when pressing n and p stay in the current window
    (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
    ;; automatically open messages when moving in the headers view
    (setq mu4e-views-auto-view-selected-message t))
  ;;
  ;; Find messages from the same sender.
  (defun my-mu4e-action-find-messages-from-same-sender (msg)
    "Extract sender from From: field and find messages from same sender

The optional and unused MSG argument is to fit into mu4e's action framework."
    (interactive)
    (let* ((from (mu4e-message-field msg :from))
           (sender-address (cdar from)))
      (mu4e-headers-search (concat "from:" sender-address))))
  ;;
  (defun my-mu4e-action-narrow-messages-to-same-sender (msg)
    "Extract sender from From: field and narrow messages to same sender

The optional and unused MSG argument is to fit into mu4e's action framework."
    (interactive)
    (let* ((from (mu4e-message-field msg :from))
           (sender-address (cdar from)))
      (mu4e-headers-search-narrow (concat "from:" sender-address))))
  ;;
  (defun my-mu4e-action-search-message-in-apple-mail (msg)
    "Extract information from the message and conduct Mail.app search.

The optional and unused MSG argument is to fit into mu4e's action framework.
The search uses the following fields: from, to, date, and subject."
    (interactive)
    ;; Use (mu4e-message-at-point) to experiment
    (let* ((from (mu4e-message-field msg :from))
           (sender-address (cdar from))
           (to-list (mu4e-message-field msg :to))
           (first-to-address (cdar to-list))
           (date (mu4e-message-field msg :date))
           ;; In 01/05/2021 format.
           (date-string (format-time-string "%m/%d/%Y" date))
           (subject (mu4e-message-field msg :subject))
           ;; Mac: How to use Spotlight’s Boolean talents to find email fast
           ;; https://www.applemust.com/mac-how-to-use-spotlights-boolean-talents-to-find-email-fast/
           (search-string (concat "kind:mail "
                                  "from:" sender-address
                                  " and "
                                  "to:" first-to-address
                                  " and "
                                  "date:" date-string
                                  ;; The subject tends to make the search yield zero results.
                                  ;; " and "
                                  ;; "subject:" subject
                                  )))
      ;; Calling Applescript from Emacs
      ;;  https://irreal.org/blog/?p=4865
      ;; How to Launch Spotlight from the Terminal
      ;;  https://superuser.com/questions/225581/how-to-launch-spotlight-from-the-terminal
      ;; AppleScript for searching Applemail
      ;;  https://discussions.apple.com/thread/8419724
      ;; How do I automate a key press in AppleScript?
      ;;  https://apple.stackexchange.com/questions/36943/how-do-i-automate-a-key-press-in-applescript
      (shell-command-to-string
       (concat "osascript -e '
tell application \"Finder\"
    activate # focus Finder
    tell application \"System Events\"
        keystroke \" \" using {command down} # Shortcut for Spotlight
        delay 1
        keystroke \""
               search-string
               "\" # enter into search box
    end tell
end tell'"))))
  ;; Actions
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Actions.html
  (setq mu4e-view-actions
        '(("capture message"    . mu4e-action-capture-message)
          ("view as pdf"        . mu4e-action-view-as-pdf)
          ("show this thread"   . mu4e-action-show-thread)
          ("browser"            . mu4e-action-view-in-browser)
          ("find same sender"   . my-mu4e-action-find-messages-from-same-sender)
          ("narrow same sender" . my-mu4e-action-narrow-messages-to-same-sender)
          ("apple mail"         . my-mu4e-action-search-message-in-apple-mail)
          ;; Example: +tag,+long tag,-oldtag
          ("tag"                . mu4e-action-retag-message)))
  ;;
;;;  Editor view configuration
  ;; New frame for a new message.
  (setq mu4e-compose-in-new-frame nil)
  (unless (version< emacs-version "27.0")
    ;; Create a new tab-bar instead of a frame on opening.
    (defun mu4e~draft-open-file (path)
      "Open the the draft file at PATH."
      (if mu4e-compose-in-new-frame
          (progn (my-tab-bar-create)
                 (find-file path))
        (find-file path)))
    ;; Drop an elscreen screen instead of a frame on sending.
    (defun mu4e-message-kill-buffer ()
      "Wrapper around `message-kill-buffer'.
It restores mu4e window layout after killing the compose-buffer."
      (interactive)
      (let ((current-buffer (current-buffer)))
        (message-kill-buffer)
        ;; Compose buffer killed
        (when (not (equal current-buffer (current-buffer)))
          ;; Restore mu4e
          (if mu4e-compose-in-new-frame
              (tab-bar-close-tab)
            (if (buffer-live-p mu4e~view-buffer)
                (switch-to-buffer mu4e~view-buffer)
              (if (buffer-live-p mu4e~headers-buffer)
                  (switch-to-buffer mu4e~headers-buffer)
                ;; if all else fails, back to the main view
                (when (fboundp 'mu4e) (mu4e)))))))))
  ;;
  ;; Include the date of the original message
  ;; https://emacs.stackexchange.com/questions/52928/add-date-to-mu4e-email-when-forwarding-or-replying
  ;; `message-citation-line-function' Function called to insert the "Whomever writes:" line.
  ;; `message-insert-formatted-citation-line' Function that inserts a formatted citation line.
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  ;; C.4.20 How can I avoid Outlook display issues?
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#Writing-messages
  ;; The string is formatted using format-spec.  The following constructs
  ;; are replaced:
  ;; %f   The full From, e.g. "John Doe <john.doe@example.invalid>".
  ;; %n   The mail address, e.g. "john.doe@example.invalid".
  ;; %N   The real name if present, e.g.: "John Doe", else fall
  ;; back to the mail address.
  ;; %F   The first name if present, e.g.: "John", else fall
  ;; back to the mail address.
  ;; %L   The last name if present, e.g.: "Doe".
  (setq message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")
  ;;
  ;; Org-mode use in composing
  ;; mu4e for Dummies
  ;;   https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/
  ;; Sending emails with math and source code
  ;;   https://vxlabs.com/2015/01/28/sending-emails-with-math-and-source-code/
  (use-package org-mu4e
    :config
    ;; Whether to do automatic org-mode => html conversion when sending messages.
    (setq org-mu4e-convert-to-html t))
  ;;
  ;; Do not drop myself from cc list
  (setq mu4e-compose-keep-self-cc t)
  ;;
  ;; org-mode's table editor minor mode
  ;; This hijack RET binding. It makes RET in flyspell popup correction unresponsive.
  ;; http://orgmode.org/manual/Orgtbl-mode.html
  ;; (add-hook 'mu4e-compose-mode-hook 'turn-on-orgtbl)
  ;;
  ;;  Sequential command for message mode
  (with-eval-after-load "sequential-command"
    (define-sequential-command message-seq-cmd--home
      message-beginning-of-line message-goto-body beginning-of-buffer seq-cmd--return)
    ;; Replace with C-a
    (add-hook 'mu4e-compose-mode-hook
              '(lambda ()
                 (local-unset-key (kbd "C-a"))
                 (local-set-key (kbd "C-a") 'message-seq-cmd--home))))
  ;;
;;;  Context
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Contexts.html
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Contexts-example.html
  ;; `mu4e-context-policy'
  ;; The policy to determine the context when entering the mu4e main view.
  (setq mu4e-context-policy nil)
  ;;
  ;; `mu4e-compose-context-policy'
  ;; Policy for determining the context when composing a new message.
  (setq mu4e-compose-context-policy 'ask)
  ;;
;;;  Conversation view
  ;; https://gitlab.com/ambrevar/mu4e-conversation/
  (use-package mu4e-conversation
    :ensure t
    :commands (global-mu4e-conversation-mode
               mu4e-conversation-toggle-view
               mu4e-conversation-send
               mu4e-conversation-cite)
    :config
    ;; View linear or tree
    (setq mu4e-conversation-print-function 'mu4e-conversation-print-linear)
    ;; Settings for the mu4e conversation view.
    (setq mu4e-conversation-own-name "Me")))


;;;
;;; gnus-dired.el
;; A.9 Attaching files with dired
;; http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
(use-package gnus-dired
  :demand t
  :hook ((dired-mode . turn-on-gnus-dired-mode))
  :config
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  ;; This overwrites `gnus-dired-mail-buffers'.
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  ;; mu4e dependency!
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  ;; Alias
  ;; Attach dired's marked files to a gnus message composition.
  (defalias 'my-mu4e-dired-attach 'gnus-dired-attach))


;;;
;;; GNUS-RELATED
;;; gnus
;; http://www.emacswiki.org/emacs/GnusGmail
(use-package gnus
  :config
  (setq gnus-select-method
        '(nnimap "gmail"
	         (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	         (nnimap-server-port "imaps")
	         (nnimap-stream ssl)))
  ;;
  (setq smtpmail-smtp-service 587)
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))
