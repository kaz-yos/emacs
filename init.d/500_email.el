;;; e-mail related                                 -*- lexical-binding: t; -*-
;; This file is public on github. Do not put sensitive information here.

;;;
;;; General configuration
;; Default in sending e-mail
;; user-full-name is a variable defined in C source code.
(setq user-full-name "Kazuki Yoshida")


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
  (setq message-sendmail-envelope-from 'header))


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
             mu4e-background)
  :hook ((after-init . mu4e-background))
  :bind (;;
         :map mu4e-view-mode-map
         ("G" . mu4e-view-go-to-url)
         ("g" . 'mu4e-headers-rerun-search))
  ;;
  :config
  (defun mu4e-background () (mu4e t))
  ;;
  ;; tell mu4e where my Maildir is
  (setq mu4e-maildir "~/.maildir")
  ;; mu binary (backend)
  (setq mu4e-mu-binary (executable-find "mu"))
  ;;
  ;;
;;;  Syncing
  ;; tell mu4e how to sync email
  ;; Defined by defcustom, thus, a dynamic variable.
  ;; Using timelimit for mbsync to limit execution time
  ;; https://groups.google.com/forum/#!topic/mu-discuss/FLz4FcECo3U
  ;; inbox-only is a group of inbox channels and does not sync other boxes
  (if (executable-find "timelimit")
      (setq mu4e-get-mail-command "timelimit -t 60 mbsync inbox-only")
    (setq mu4e-get-mail-command "mbsync inbox-only"))
  ;;
  ;; Define a function to list visible buffer names.
  (defun visible-buffer-names ()
    "Return a list of visible buffer names"
    (let* ((all-buffer-names (mapcar (function buffer-name) (buffer-list))))
      (seq-filter 'get-buffer-window
                  all-buffer-names)))
  ;; Function to check mu4e visibility.
  (defun mu4e-buffer-visible-p ()
    "Return non-nil if any buffer with a name containing mu4e has a window"
    (seq-filter (lambda (str) (string-match "*mu4e" str))
                (visible-buffer-names)))
  ;;
  ;; Define a function to change mbsync behavior when called interactively
  (defun modify-mu4e-get-mail-command (run-in-background)
    "Manipulate mu4e-get-mail-command depending on interactive status"
    ;; "P" is for universal argument
    ;; http://ergoemacs.org/emacs/elisp_universal_argument.html
    (interactive "P")
    (cond
     ;; If interactive,
     ((called-interactively-p 'interactive)
      ;; Conduct abbreviated operations.
      (setq mu4e-hide-index-messages t)
      (setq mu4e-cache-maildir-list t)
      (setq mu4e-index-cleanup nil)
      (setq mu4e-index-lazy-check t)
      ;; Inbox-only
      (if (executable-find "timelimit")
          (setq mu4e-get-mail-command "timelimit -t 60 mbsync inbox-only")
        (setq mu4e-get-mail-command "mbsync inbox-only")))
     ;;
     ;; If any mu4e windows are active, abbreviate operations.
     ;; This happens even if update is running non-interactively.
     ((mu4e-buffer-visible-p)
      ;; Conduct abbreviated operations.
      (setq mu4e-hide-index-messages t)
      (setq mu4e-cache-maildir-list t)
      (setq mu4e-index-cleanup nil)
      (setq mu4e-index-lazy-check t)
      ;; Inbox-only
      (if (executable-find "timelimit")
          (setq mu4e-get-mail-command "timelimit -t 60 mbsync inbox-only")
        (setq mu4e-get-mail-command "mbsync inbox-only")))
     ;;
     ;; Otherwise,
     (t
      ;; Conduct thorough operations without messages
      (setq mu4e-hide-index-messages t)
      (setq mu4e-cache-maildir-list nil)
      (setq mu4e-index-cleanup t)
      (setq mu4e-index-lazy-check nil)
      ;; All boxes
      (if (executable-find "timelimit")
          (setq mu4e-get-mail-command "timelimit -t 300 mbsync all")
        (setq mu4e-get-mail-command "mbsync all")))))
  ;; :before advice to mainpulate mu4e-get-mail-command variable
  (advice-add 'mu4e-update-mail-and-index :before #'modify-mu4e-get-mail-command)
  ;; (advice-remove 'mu4e-update-mail-and-index #'modify-mu4e-get-mail-command)
  ;;
  ;; Function to sync all folders
  (defun mu4e-update-mail-and-index-all (run-in-background)
    "Update email with more extensive folder syncing"
    (interactive "P")
    (setq mu4e-hide-index-messages t)
    (setq mu4e-cache-maildir-list nil)
    (setq mu4e-index-cleanup t)
    (setq mu4e-index-lazy-check nil)
    ;; All boxes
    (if (executable-find "timelimit")
        (setq mu4e-get-mail-command "timelimit -t 300 mbsync all")
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
  ;; Update interval. If nil, don't update automatically.
  (setq mu4e-update-interval nil)
  ;; Do not occupy the minibuffer with "Indexing..."
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/General.html
  (setq mu4e-hide-index-messages nil)
  ;; Whether to cache the list of maildirs
  (setq mu4e-cache-maildir-list t)
  ;; Whether to run a cleanup phase after indexing (avoids ghost messages)
  (setq mu4e-index-cleanup t)
  ;; Date stamp check only; miss message that are modified outside mu
  (setq mu4e-index-lazy-check nil)
  ;;
  ;; The dreaded "Waiting for message..." message and what to do about it
  ;; http://mu-discuss.narkive.com/VpWEtZ7w/the-dreaded-waiting-for-message-message-and-what-to-do-about-it
  ;; This hook does not exist now.
  ;; (add-hook 'mu4e~proc-start-hook
  ;;           '(lambda ()
  ;;              (message "Now running the 'killall mu' hook!")
  ;;              (shell-command "killall mu")
  ;;              (sleep-for 0 250)))
  ;;
  ;;
;;;  Dynamic folder selection (configured elsewhere)
  ;;
  ;;
;;;  Main view configuration
  ;;
  ;;
;;;  Header view configuration
  (setq mu4e-split-view 'vertical)
  ;; number of columns
  (setq mu4e-headers-visible-columns 100)
  (defun my-mu4e-set-header-columns-half-frame ()
    "Make header split frame into half in mu4e split view"
    (interactive)
    (setq mu4e-headers-visible-columns
          ;; Set to 1/2 of frame width in default characters
          (/ (/ (frame-text-width) (frame-char-width)) 2)))
  (add-hook 'mu4e-headers-mode-hook 'my-mu4e-set-header-columns-half-frame)
  ;; Delete other windows before entering message from header.
  (advice-add 'mu4e-headers-view-message
              :before 'delete-other-windows)
  ;; X-Keywords is the gmail compatible tag name.
  (setq mu4e-action-tags-header "X-Keywords")
  ;; The header date format for messages received yesterday and before.
  (setq mu4e-headers-date-format "%Y-%m-%d")
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
  (use-package mu4e-contrib
    :config
    (setq mu4e-html2text-command 'mu4e-shr2text)
    (setq shr-color-visible-luminance-min 60)
    (setq shr-color-visible-distance-min 5)
    (setq shr-use-colors nil)
    (advice-add #'shr-colorize-region
                :around (defun shr-no-colourise-region
                            (&rest ignore))))
  ;;
  ;; pdf view
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html
  ;; https://github.com/djcb/mu/issues/443
  ;; http://stackoverflow.com/questions/24194357/glib-h-file-not-found-on-max-osx-10-9
  (setq mu4e-msg2pdf nil)
  ;; Default directory for saving attachments.
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))
  ;;
  ;; Find messages from the same sender.
  (defun my-mu4e-action-find-messages-from-same-sender (msg)
    "Extract sender from From: field and find messages from same sender

The optional and unused msg argument is to fit into mu4e's action framework."
    (interactive)
    (let* ((from (mu4e-message-field msg :from))
           (sender-address (cdar from)))
      (mu4e-headers-search (concat "from:" sender-address))))
  ;;
  (defun my-mu4e-action-narrow-messages-to-same-sender (msg)
    "Extract sender from From: field and narrow messages to same sender

The optional and unused msg argument is to fit into mu4e's action framework."
    (interactive)
    (let* ((from (mu4e-message-field msg :from))
           (sender-address (cdar from)))
      (mu4e-headers-search-narrow (concat "from:" sender-address))))
  ;;
  ;; Actions
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Actions.html
  (setq mu4e-view-actions
        '(("capture message"    . mu4e-action-capture-message)
          ("view as pdf"        . mu4e-action-view-as-pdf)
          ("show this thread"   . mu4e-action-show-thread)
          ("browser"            . mu4e-action-view-in-browser)
          ("find same sender"   . my-mu4e-action-find-messages-from-same-sender)
          ("narrow same sender" . my-mu4e-action-narrow-messages-to-same-sender)
          ;; Example: +tag,+long tag,-oldtag
          ("tag"                . mu4e-action-retag-message)))
  ;;
;;;  Editor view configuration
  ;; New frame for a new message.
  (setq mu4e-compose-in-new-frame t)
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
  ;; Do not drop myself from cc list
  (setq mu4e-compose-keep-self-cc t)
  ;; Flyspell
  (add-hook 'mu4e-compose-mode-hook 'turn-on-flyspell)
  ;;
  ;; Whether to include a date header when starting to draft
  (setq mu4e-compose-auto-include-date t)
  ;;
  ;; Drop the date header before sending.
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
  ;; org-mode's table editor minor mode
  ;; This hijack RET binding. It makes RET in flyspell popup correction unresponsive.
  ;; http://orgmode.org/manual/Orgtbl-mode.html
  ;; (add-hook 'mu4e-compose-mode-hook 'turn-on-orgtbl)
  ;;
  ;;  Sequential command for message mode
  (define-sequential-command message-seq-cmd--home
    message-beginning-of-line message-goto-body beginning-of-buffer seq-cmd--return)
  ;; Replace with C-a
  (add-hook 'mu4e-compose-mode-hook '(lambda ()
                                       (local-unset-key (kbd "C-a"))
                                       (local-set-key (kbd "C-a") 'message-seq-cmd--home))))


;;;
;;; gnus-dired.el
;; A.9 Attaching files with dired
;; http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
(use-package gnus-dired
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
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))


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
