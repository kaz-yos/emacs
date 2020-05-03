;;; 500_opening-special-files-related.el ---         -*- lexical-binding: t; -*-

;;;
;;; vlf: View Large Files
;; https://github.com/m00natic/vlfi
(use-package vlf-setup
  :ensure vlf
  :config
  ;; Maximum size of file above which a confirmation is requested.
  (setq large-file-warning-threshold 10000000)
  ;; Determines when `vlf' will be offered on opening files.
  (setq vlf-application 'ask))


;;;
;;; open-junk-file.el
;; https://github.com/rubikitch/open-junk-file
(use-package open-junk-file
  :ensure t
  :if (or (file-exists-p "~/junk/")
          (file-exists-p "~/Dropbox/junk/"))
  ;; Use the version with counsel.
  :bind (("C-x C-z" . my-open-junk-file))
  :config
  ;; This work around is necessary when ivy is used.
  ;; Taken from spacemacs.
  ;; https://github.com/syl20bnr/spacemacs/blob/582f9aa45c2c90bc6ec98bccda33fc428e4c6d48/layers/%2Bspacemacs/spacemacs-ui/packages.el#L157
  (defun my-open-junk-file (&optional arg)
    "Open junk file using ivy.
When ARG is non-nil search in junk files."
    (interactive "P")
    (let* ((fname (format-time-string open-junk-file-format (current-time)))
           (rel-fname (file-name-nondirectory fname))
           (junk-dir (file-name-directory fname))
           ;; Name of default directory of current buffer.
           ;; This must be replaced with the junk-dir for counsel to work correctly.
           (default-directory junk-dir))
      (counsel-find-file rel-fname)))
  ;;
  ;; File format to put junk files with directory.
  (cond
   ((file-exists-p "~/junk/")
    (setq open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S."))
   ((file-exists-p "~/Dropbox/junk/")
    (setq open-junk-file-format "~/Dropbox/junk/%Y/%m/%d-%H%M%S.")))
  ;; Whether to immediately create a directory for the junk file
  (setq open-junk-file-make-directory t))


;;;
;;; pdf-tools.el
;; https://github.com/politza/pdf-tools
;; Although OS X is not officially supported, it has been reported to have been
;; successfully compiled. You will need to install poppler which you can get with
;; homebrew via
;; $ brew install poppler automake
;; Note: When using elpa pdf-tools, there is not need to install Homebrew pdf-tools.
;;
;; When it's not working try running epdfinfo directly to look for errors.
;; $ epdfinfo
;;
;;;  Issue with poppler 0.60.1
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
;;
;;;  Permission issue
;; /bin/sh: /Users/kazuki/.emacs.d/elpa/pdf-tools-20180109.1234/build/server/autobuild: Permission denied
;; Executable permission was not granted.
;; chmod u+x /Users/kazuki/.emacs.d/elpa/pdf-tools-20180109.1234/build/server/autobuild
;;
;;;  Issue with poppler 0.64.0
;; /Users/kazuki/.emacs.d/elpa/pdf-tools-20180109.1234/build/server/autobuild -i /usr/local/bin/
;; ...
;; ---------------------------
;;  Configuring and compiling
;; ---------------------------
;; ./configure -q --bindir=/usr/local/bin/ && make -s
;;
;; Is case-sensitive searching enabled ?     yes
;; Is modifying text annotations enabled ?   yes
;; Is modifying markup annotations enabled ? yes
;;
;; poppler-hack.cc:88:16: error: cannot initialize a variable of type 'GooString *' with an rvalue of type 'const GooString *'
;;     GooString *state = a->annot->getAppearState ();
;;                ^       ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; poppler-hack.cc:108:19: error: assigning to 'GooString *' from incompatible type 'const GooString *'
;;     text = annot->getDate ();
;;            ~~~~~~~^~~~~~~~~~
;; 2 errors generated.
;; make[1]: *** [epdfinfo-poppler-hack.o] Error 1
;; make: *** [all] Error 2
;; ===========================
;;      Build failed.  ;o(
;; ===========================
;; Note: maybe try the '-d' option.
;;
;; brew switch poppler 0.63.0_1
;; /Users/kazuki/.emacs.d/elpa/pdf-tools-20180109.1234/build/server/autobuild -i /usr/local/bin/
;; ===========================
;;    Build succeeded. :O)
;; ===========================
;;
;; pdf-tools-20190701.202/build/server/autobuild supports macOS
;; and uses brew to satisfy dependencies.
;; ---------------------------
;; Configuring and compiling
;; ---------------------------
;; autoreconf -i
;; configure.ac:15: installing './ar-lib'
;; configure.ac:11: installing './compile'
;; configure.ac:78: installing './config.guess'
;; configure.ac:78: installing './config.sub'
;; configure.ac:6: installing './install-sh'
;; configure.ac:6: installing './missing'
;; Makefile.am: installing './depcomp'
;; ./configure -q --bindir=/Users/kazuki/.emacs.d/elpa/pdf-tools-20190701.202/ && make -s
;; configure: error: Package requirements (poppler-glib >= 0.16.0) were not met:
;; Package 'libffi', required by 'gobject-2.0', not found
;; Consider adjusting the PKG_CONFIG_PATH environment variable if you
;; installed software in a non-standard prefix.
;; Alternatively, you may set the environment variables poppler_glib_CFLAGS
;; and poppler_glib_LIBS to avoid the need to call pkg-config.
;; See the pkg-config man page for more details.
;; ===========================
;; Build failed.  ;o(
;; ===========================
;; Note: maybe try the '-d' option.
;;
;; brew reinstall libffi
;; Same error
;;
;; 2019-09-22
;; -*- mode: compilation; default-directory: "~/.emacs.d/elpa/pdf-tools-20190918.1715/build/server/" -*-
;; Comint started at Sun Sep 22 04:30:22
;; /Users/kazuki/.emacs.d/elpa/pdf-tools-20190918.1715/build/server/autobuild -i /Users/kazuki/.emacs.d/elpa/pdf-tools-20190918.1715/
;; PDFView finished at Sun Sep 22 04:30:43
;; epdfinfo now in ~/.emacs.d/elpa/pdf-tools-20190918.1715/
(use-package pdf-tools
  :ensure t
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
  ;; Define autoloads necessary when using (package-initialize t) (no activation)
  ;; Taken from the body of `package-initialize'
  (package-activate 'pdf-tools)
  ;;
  (use-package pdf-view
    :config
    ;; Fractional amount of resizing of one resize command.
    (setq pdf-view-resize-factor 1.05))
  ;;
  ;; Filename of the epdfinfo executable.
  (setq pdf-info-epdfinfo-program
        (or (executable-find "epdfinfo")
            (concat (car (last (file-expand-wildcards (concat
                                                       user-emacs-directory
                                                       "elpa/pdf-tools*"))))
                    "/"
                    "epdfinfo")))
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
  :ensure t
  :mode (("\\.epub" . nov-mode)))


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


;;;
;;; csv-mode.el
;; http://emacswiki.org/emacs/CsvMode
(use-package csv-mode
  :ensure t
  :mode ("\\.csv" . csv-mod))
