;;; 500_opening-special-files-related.el ---         -*- lexical-binding: t; -*-

;;;
;;; vlf: View Large Files
;; https://github.com/m00natic/vlfi
(use-package vlf
  :commands (vlf)
  :config
  (vlf-setup))


;;;
;;; open-junk-file.el for creation of permanent test files
(use-package open-junk-file
  :bind (("C-x C-z" . open-junk-file))
  :config
  (setq open-junk-file-directory "~/junk/%Y/%m/%d-%H%M%S."))


;;;
;;; pdf-tools.el
;; https://github.com/politza/pdf-tools
;; brew install poppler automake
;; Although OS X is not officially supported, it has been reported to have been
;; successfully compiled. You will need to install poppler which you can get with
;; homebrew via brew install poppler automake (recipe below takes care of this)
;;
;; http://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
;; Install epdfinfo via 'brew install homebrew/emacs/pdf-tools' and then install the
;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;; pdf-tools package using Emacs package system. If things get messed
;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;; pdf-tools package and reinstall both as at the start.
;;
;; Homebrew recipe moved to:
;; https://github.com/dunn/homebrew-emacs
(use-package pdf-tools
  ;; The deferring configuration was take from the following repository.
  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-pdf.el
  :mode (("\\.pdf\\'" . pdf-view-mode))
  ;;
  :config
  ;; Whether PDF Tools should handle upgrading itself.
  ;; Up grading should be via Homebrew
  (setq pdf-tools-handle-upgrades nil)
  ;;
  ;; Filename of the epdfinfo executable installed via Homebrew
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  ;;
  ;; Install PDF-Tools in all current and future PDF buffers.
  (pdf-tools-install)
  ;;
  (add-hook 'pdf-view-mode-hook #'turn-on-auto-revert-mode))


;;;
;;; IMAGE-RELATED
;; https://emacs.stackexchange.com/questions/2433/shrink-zoom-scale-images-in-image-mode
;;;  eimp.el
;; https://www.emacswiki.org/emacs/eimp.el
(use-package eimp
  :commands (eimp-mode)
  :init
  (add-hook 'image-mode-hook 'eimp-mode))
