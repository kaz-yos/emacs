;;; e-mail related                                 -*- lexical-binding: t; -*-


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
;;
;; Install infrastructure
;;
;; mu along with mu4e
;; make sure (which emacs gives the path to a new emacs (23+)
;; https://github.com/Homebrew/homebrew/issues/16504#issuecomment-11394215
;; EMACS=$(which emacs) brew install mu --with-emacs --HEAD
;; ./configure --prefix=/usr/local/Cellar/mu/HEAD --with-lispdir=/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu
;;
;; offlineimap
;; brew install offlineimap
;; * minimal configuration:
;;     cp -n /usr/local/etc/offlineimap.conf.minimal ~/.offlineimaprc
;; * advanced configuration:
;;     cp -n /usr/local/etc/offlineimap.conf ~/.offlineimaprc
;; To have launchd start offlineimap at login:
;;   ln -sfv /usr/local/opt/offlineimap/*.plist ~/Library/LaunchAgents
;; Then to load offlineimap now:
;;   launchctl load ~/Library/LaunchAgents/homebrew.mxcl.offlineimap.plist
;;
;;
;;; mu4e.el
;; Reading IMAP Mail in Emacs on OSX
;; http://www.ict4g.net/adolfo/notes/2014/12/27/EmacsIMAP.html
(use-package mu4e
  :init
  ;; Check for mu4e directory
  (let ((mu4e-dir
         "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e/"))
    (when (file-exists-p mu4e-dir)
      (add-to-list 'load-path mu4e-dir)))
  :config
  ;; tell mu4e where my Maildir is
  (setq mu4e-maildir "~/Documents/.maildir")
  ;; mu binary (backend)
  (setq mu4e-mu-binary "/usr/local/bin/mu")
  ;; tell mu4e how to sync email
  ;; Using timelimit for mbsync to limit execution time
  ;; https://groups.google.com/forum/#!topic/mu-discuss/FLz4FcECo3U
  (if (file-exists-p "/usr/local/bin/timelimit")
      (setq mu4e-get-mail-command "timelimit -t 120 mbsync -Va")
    (setq mu4e-get-mail-command "mbsync -Va"))
  ;; tell mu4e to use w3m for html rendering
  (setq mu4e-html2text-command "w3m -T text/html")
  ;; taken from mu4e page to define bookmarks
  (add-to-list 'mu4e-bookmarks
               '("size:5M..500M"       "Big messages"     ?b))
  ;; mu4e requires to specify drafts, sent, and trash dirs
  ;; relative to mu4e-maildir
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-trash-folder "/trash")
  ;;
  ;; Sending
  ;; use msmtp
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  ;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't))



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
