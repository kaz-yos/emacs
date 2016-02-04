;;; e-mail related                                 -*- lexical-binding: t; -*-


;;; mu4e-related
;; http://www.djcbsoftware.nl/code/mu/mu4e.html
;; https://www.emacswiki.org/emacs/mu4e
;;
;; E-mail in Emacs with mu4e on OS X
;; http://cmacr.ae/blog/2015/01/25/email-in-emacs/
;; Manage your gmail account in emacs with mu4e
;; https://gist.github.com/areina/3879626
;; http://blog.danielgempesaw.com/post/43467552978/installing-mu-and-mu4e-with-homebrew-with-emacs
;; installing mu and mu4e with homebrew with emacs from emacsforosx
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
