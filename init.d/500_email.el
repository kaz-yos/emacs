;;; e-mail related                                 -*- lexical-binding: t; -*-
;; This file is public on github. Do not put sensitive information here.

;;;
;;; General configuration
;; Default in sending e-mail
(setq user-full-name "Kazuki Yoshida")

;;;  SMTP configuration
;; use msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)


;;;
;;; mu4e-related
;; http://www.djcbsoftware.nl/code/mu/mu4e.html
;; https://www.emacswiki.org/emacs/mu4e
;;
;; E-mail in Emacs with mu4e on OS X
;; http://cmacr.ae/blog/2015/01/25/email-in-emacs/
;; Manage your gmail account in emacs with mu4e
;; https://gist.github.com/areina/3879626
;; installing mu and mu4e with homebrew with emacs from emacsforosx
;; http://blog.danielgempesaw.com/post/43467552978/installing-mu-and-mu4e-with-homebrew-with-emacs
;; MU4E TUTORIALS
;; http://pragmaticemacs.com/mu4e-tutorials/
;; The Ultimate Emailing Agent with Mu4e and Emacs
;; http://tech.memoryimprintstudio.com/the-ultimate-emailing-agent-with-mu4e-and-emacs/
;;
;; Install infrastructure
;;
;; Install HEAD mu along with mu4e following below
;; make sure which emacs gives the path to a new emacs (23+)
;; https://github.com/Homebrew/homebrew/issues/16504#issuecomment-11394215
;; EMACS=$(which emacs) brew install mu --with-emacs --HEAD
;; ./configure --prefix=/usr/local/Cellar/mu/HEAD --with-lispdir=/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu
;;
;; These unix tools also have to be installed and configured elsewhere.
;; mbsync (isync package) for IMAP-Maildir syncing
;; msmtp for sending messages
;; pass and gpg for encrypted password handling (instead macOS security command in use)
;;
;;
;;; mu4e.el
;; Reading IMAP Mail in Emacs on OSX
;; http://www.ict4g.net/adolfo/notes/2014/12/27/EmacsIMAP.html
;; Execute the following before first invocation
;; mu index --maildir=~/.maildir
(use-package mu4e
  :commands (mu4e)
  :init
  ;; Check for mu4e directory before invoking all the following
  (let ((mu4e-dir "/usr/local/Cellar/mu/HEAD-1f232b6/share/emacs/site-lisp/mu/mu4e/"))
    (when (file-exists-p mu4e-dir)
      (add-to-list 'load-path mu4e-dir)))
  ;;
  :config
  ;; tell mu4e where my Maildir is
  (setq mu4e-maildir "~/.maildir")
  ;; mu binary (backend)
  (setq mu4e-mu-binary "/usr/local/bin/mu")
  ;; tell mu4e how to sync email
  ;; Using timelimit for mbsync to limit execution time
  ;; https://groups.google.com/forum/#!topic/mu-discuss/FLz4FcECo3U
  (if (executable-find "timelimit")
      (setq mu4e-get-mail-command "timelimit -t 120 mbsync -Va")
    (setq mu4e-get-mail-command "mbsync -Va"))
  ;; Change file UID when moving (necessary for mbsync, but not for offlineimap)
  ;; https://groups.google.com/forum/m/#!topic/mu-discuss/8c9LrYYpxjQ
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/General.html
  (setq mu4e-change-filenames-when-moving t)
  ;; Update interval
  (setq mu4e-update-interval (* 60 5))
  ;; Do not occupy the minibuffer with "Indexing..."
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/General.html
  (setq mu4e-hide-index-messages t)
  ;; Default directory for saving attachments.
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/"))
  ;;
;;;  Dynamic folder selection (configured elsewhere)
  ;;
  ;;
;;;  Main view configuration
  ;;
;;;  Header view configuration
  (setq mu4e-split-view 'vertical)
  ;; ‘apply’ automatically apply the marks before doing anything else
  (setq mu4e-headers-leave-behavior 'apply)
  (setq mu4e-headers-visible-columns 100)
  (setq mu4e-headers-results-limit 500)
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
  ;; Faces
  ;; mu4e-trashed-face
  ;;
;;;  Message view configuration
  ;; Whether to automatically display attached images in the message
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  ;; Actions
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Actions.html
  (setq mu4e-view-actions
        '(("capture message"  . mu4e-action-capture-message)
          ("view as pdf"      . mu4e-action-view-as-pdf)
          ("show this thread" . mu4e-action-show-thread)
          ("browser"          . mu4e-action-view-in-browser)))
  ;;
  ;; Displaying rich-text messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
  (setq mu4e-view-prefer-html nil)
  ;; html converters use first one that exists
  (cond ((executable-find "/usr/local/bin/html2text")
         (setq mu4e-html2text-command "/usr/local/bin/html2text -utf8 -nobs -width 72"))
        ((executable-find "w3m")
         (setq mu4e-html2text-command "w3m -T text/html"))
        ((executable-find "textutil")
         (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"))
        ;; This one gives live links, but seems to be slow.
        (t (progn (require 'mu4e-contrib)
                  (setq mu4e-html2text-command 'mu4e-shr2text))))
  ;; pdf view
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html
  ;; https://github.com/djcb/mu/issues/443
  ;; http://stackoverflow.com/questions/24194357/glib-h-file-not-found-on-max-osx-10-9
  (setq mu4e-msg2pdf nil)
  ;;
;;;  Editor view configuration
  ;; Do not drop myself from cc list
  (setq mu4e-compose-keep-self-cc t)
  ;; Always CC myself
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
  (add-hook 'mu4e-compose-mode-hook
            (defun my-add-header ()
              "Add CC: and Bcc: to myself header."
              (save-excursion (message-add-header
                               (concat "CC: " "\n")
                               (concat "Bcc: " user-mail-address "\n")))))
  ;;
;;; mu4e-maildirs-extension.el
  ;; Show mu4e maildirs summary in mu4e-main-view
  ;; https://github.com/agpchil/mu4e-maildirs-extension
  (require 'mu4e-maildirs-extension)
  (mu4e-maildirs-extension)
  ;;
;;; mu4e-alert.el
  ;; Notification for mu4e
  ;; https://github.com/iqbalansari/mu4e-alert
  (require 'mu4e-alert)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (when (file-exists-p "/usr/local/bin/terminal-notifier")
    (setq alert-notifier-command "/usr/local/bin/terminal-notifier")
    (mu4e-alert-set-default-style 'notifier)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  ;; Interesting mail only
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed")))



;;;
;;; GNUS-RELATED
;;; gnus
;; http://www.emacswiki.org/emacs/GnusGmail
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
