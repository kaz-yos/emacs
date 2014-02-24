;;; auto-install.el

;;; Auto-intall	; This is causing problem !!! 2013-03-04
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
  (setq auto-install-use-wget t)
  )
;; This has to be done??
;; sudo ln -s `which wget` /usr/bin/wget
;; ;;
;; THIS FREEZES EMACS IF EMACSWIKI IS DONW!!!!!! Defined at the end of this file.
;; (auto-install-update-emacswiki-package-name t)
;; INSTALL-elisp.el compatilibity
(auto-install-compatibility-setup)
;; ediff associated buffers in one frame
(setq ediff-windows-setup-function 'ediff-setup-windows-plain)

;; This is placed at the end as it freezes if EmacsWIki is not responding.
(auto-install-update-emacswiki-package-name t) ; THIS FREEZES EMACS IF EMACSWIKI IS DONW!!!!!!
;;; end of configuration

