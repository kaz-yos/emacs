;;; 500_opening-special-files-related.el ---         -*- lexical-binding: t; -*-

;;;
;;; vlf: View Large Files
;; https://github.com/m00natic/vlfi
(use-package vlf-setup
  :config
  ;; Maximum size of file above which a confirmation is requested.
  (setq large-file-warning-threshold 10000000)
  ;; Determines when `vlf' will be offered on opening files.
  (setq vlf-application 'ask))


;;;
;;; open-junk-file.el for creation of permanent test files
(use-package open-junk-file
  :bind (("C-x C-z" . open-junk-file))
  :config
  (setq open-junk-file-directory "~/junk/%Y/%m/%d-%H%M%S."))


;;;
;;; pdf-tools.el
;; https://github.com/politza/pdf-tools
;; Although OS X is not officially supported, it has been reported to have been
;; successfully compiled. You will need to install poppler which you can get with
;; homebrew via
;; $ brew install poppler automake
;;
;; When it's not working try running epdfinfo directly to look for errors.
;; $ epdfinfo
;;
;; Issue with poppler 0.60.1
;; /usr/local/Cellar/poppler/0.60.1 (443 files, 18.4MB) *
;; Poured from bottle on 2017-10-06 at 12:35:58
;; $ epdfinfo
;; dyld: Library not loaded: /usr/local/opt/poppler/lib/libpoppler.70.dylib
;; Referenced from: /usr/local/bin/epdfinfo
;; Reason: image not found
;;
;; Solved by creating a symlink.
;; $ cd /usr/local/opt/poppler/lib/
;; $ ln -s libpoppler.71.dylib libpoppler.70.dylib
(use-package pdf-tools
  :if (display-graphic-p)
  ;; The deferring configuration was take from the following repository.
  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-pdf.el
  :commands (my-revert-pdf)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind (:map pdf-view-mode-map
              ("g" . my-revert-pdf)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation)
              ("D" . pdf-annot-delete)
              ;; use normal isearch
              ("C-s" . isearch-forward))
  ;;
  :config
  ;; These are necessary to create autoloads if not using (package-initialize)
  (use-package pdf-history
    :commands (pdf-history-minor-mode))
  (use-package pdf-occur
    ;; These are required by pdf-tools-install.
    :commands (pdf-occur-global-minor-mode))
  ;;
  ;; Filename of the epdfinfo executable.
  (setq pdf-info-epdfinfo-program (executable-find "epdfinfo"))
  ;;
  ;; Install PDF-Tools in all current and future PDF buffers.
  ;; https://github.com/politza/pdf-tools/issues/72
  (pdf-tools-install)
  ;;
  ;; http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools/
  ;; http://pragmaticemacs.com/emacs/more-pdf-tools-tweaks/
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)

  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;;
  (defun kill-file-associated-buffer-and-reopen-file ()
    "Kill the buffer if its file associated, and reopen the file."
    (let ((file buffer-file-name))
      (when file
        (kill-buffer)
        (find-file file))))
  ;;
  (defun my-revert-pdf ()
    "Revert if local. Kill and reopen if remote."
    (interactive)
    (cond
     ;; If remote kill buffer and reopen.
     ((file-remote-p buffer-file-name)
      (kill-file-associated-buffer-and-reopen-file))
     ;; If not remote, just revert
     (t (revert-buffer)))))


;;;
;;; ePUB-related
;;; nov.el
;; https://github.com/wasamasa/nov.el
(use-package nov
  :mode (("\\.epub" . nov-mode)))


;;;
;;; IMAGE-RELATED
;; https://emacs.stackexchange.com/questions/2433/shrink-zoom-scale-images-in-image-mode

;;;  eimp.el
;; https://www.emacswiki.org/emacs/eimp.el
;; Beaware this immediately manipulates the original image if used with auto-save.
(use-package eimp
  :disabled t
  :commands (eimp-mode)
  :init
  (add-hook 'image-mode-hook 'eimp-mode))


;;;  picpocket.el
;; https://github.com/johanclaesson/picpocket
(use-package picpocket
  :if (display-graphic-p)
  :commands (picpocket))


;;;
;;; COMPRESSED FILE-RELATED
;;;  jka-compr.el
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/jka-compr.el
;; https://www.emacswiki.org/emacs/MacOSXPlist
(use-package jka-compr
  :config
  ;; Allow editing of binary .plist files.
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])
  ;; It is necessary to perform an update!
  (jka-compr-update))
