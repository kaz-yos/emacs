;;; auto-install.el

;;; Auto-intall
;; http://www.emacswiki.org/AutoInstall
;; http://www.emacswiki.org/emacs/auto-install.el
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
;;
;; emacs customization book by rubikitch
;; EmacsWiki added for auto-completion list at startup
;; http://spiri-tua-lism.com/?p=451
(when (eq system-type 'darwin)
  ;; Mac-only
  (setq auto-install-use-wget t))
;; This has to be done??
;; sudo ln -s `which wget` /usr/bin/wget
;; ;;
;; THIS FREEZES EMACS IF EMACSWIKI IS DONW!!!!!! Defined at the end of this file.
;; INSTALL-elisp.el compatilibity
(auto-install-compatibility-setup)
;; ediff associated buffers in one frame
(setq ediff-windows-setup-function 'ediff-setup-windows-plain)
;;
;;
;; THIS FREEZES EMACS IF EMACSWIKI IS DONW!!!!!! (not necessary)
;; (auto-install-update-emacswiki-package-name t)
