;;; Buffere-related configurations  -*- lexical-binding: t; -*-


;;;
;;; Unique buffer names
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;;
;;; autorevert.el
(use-package autorevert
  :commands (auto-revert-mode)
  :bind (:map my-key-map
              ("a" . auto-revert-mode))
  ;;
  :config
  ;; Active in all buffers
  (setq global-auto-revert-mode nil)
  ;; Even in non-file buffers
  (setq global-auto-revert-non-file-buffers t)
  ;; VC status change is also captured
  (setq auto-revert-check-vc-info t)
  ;; No ARev in mode-line
  ;; (setq auto-revert-mode-text "")
  )



;;;
;;; ibuffer.el
;; http://www.emacswiki.org/emacs/IbufferMode
;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(use-package ibuffer
  :commands (ibuffer)
  :init
  (defalias 'list-buffers 'ibuffer)
  :config
;;;  Sorting
  ;; http://mytechrants.wordpress.com/2010/03/25/emacs-tip-of-the-day-start-using-ibuffer-asap/
  ;; (setq ibuffer-default-sorting-mode 'major-mode)
  ;; https://github.com/pd/dotfiles/blob/master/emacs.d/pd/core.el
  (setq ibuffer-default-sorting-mode 'filename/process)
  ;; Drop empty groups
  (setq ibuffer-show-empty-filter-groups nil)
  ;;
;;;  Format
  ;; A list of ways to display buffer lines.
  ;; This variable has the form
  ;; ((COLUMN COLUMN ...) (COLUMN COLUMN ...) ...)
  ;; A COLUMN can be any of the following:
  ;;
  ;; SYMBOL - A symbol naming the column.  Predefined columns are:
  ;; mark modified read-only locked name size mode process filename
  ;; When you define your own columns using define-ibuffer-column, just
  ;; use their name like the predefined columns here.  This entry can
  ;; also be a function of two arguments, which should return a string.
  ;; The first argument is the buffer object, and the second is the mark
  ;; on that buffer.
  ;;
  ;; "STRING" - A literal string to display.
  ;;
  ;; (SYMBOL MIN-SIZE MAX-SIZE &optional ALIGN ELIDE) - SYMBOL is a
  ;; symbol naming the column, and MIN-SIZE and MAX-SIZE are integers (or
  ;; functions of no arguments returning an integer) which constrict the
  ;; size of a column.  If MAX-SIZE is -1, there is no upper bound.  The
  ;; default values are 0 and -1, respectively.  If MIN-SIZE is negative,
  ;; use the end of the string.  The optional element ALIGN describes the
  ;; alignment of the column; it can be :left, :center or :right.  The
  ;; optional element ELIDE describes whether or not to elide the column
  ;; if it is too long; valid values are :elide and nil.  The default is
  ;; nil (don't elide).
  ;;
  (setq ibuffer-formats
        '((mark modified read-only
                " "
                ;; Buffer name
                ;; (SYMBOL MIN-SIZE MAX-SIZE &optional ALIGN ELIDE)
                (name 18 18 :left :elide)
                " "
                ;; Human-readable buffer size (size-h) instead of size.
                (size-h 9 -1 :right)
                " "
                ;; Mode
                (mode 10 10 :left :elide)
                " "
                filename-and-process)))
;;;   Human readable buffer size entry
  ;; http://www.emacswiki.org/emacs/IbufferMode#toc12
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  ;;
;;;  Grouping
  ;; http://www.emacswiki.org/emacs/IbufferMode#toc6
  ;; An alist of filtering groups to switch between.
  ;; This variable should look like (("STRING" QUALIFIERS)
  ;;                                 ("STRING" QUALIFIERS) ...), where
  ;; QUALIFIERS is a list of the same form as `ibuffer-filtering-qualifiers'.
  (setq ibuffer-saved-filter-groups
        '(("default"
           ;; Directories
           ("DIRED" (mode . dired-mode))
           ;; Authoring
           ("ORG" (mode . org-mode))
           ("TeX"    (or
                      (mode . TeX-mode)
                      (mode . LaTeX-mode)))
           ;; Programming languages
           ("ESS"   (or
                     (mode . ess-mode)
                     (mode . inferior-ess-mode)
                     (mode . Rd-mode)))
           ("CLOJURE" (or
                       (mode . clojure-mode)
                       (name . "^\\*cider-")
                       (name . "^\\*nrepl-")))
           ("SLIME" (or
                     (mode . lisp-mode)
                     (name . "^\\*slime")
                     (name . "*inferior-lisp*")))
           ("SCHEME" (or
                      (mode . scheme-mode)
                      (mode . inferior-scheme-mode)
                      (mode . geiser-repl-mode)))
           ("HASKELL" (or
                       (mode . haskell-mode)
                       (mode . inferior-haskell-mode)))
           ("PYTHON" (or
                      (mode . python-mode)
                      (mode . inferior-python-mode)
                      (mode . ein:notebooklist-mode)
                      (mode . ein:notebook-multilang-mode)))
           ("ML" (or
                  (mode . sml-mode)
                  (mode . inferior-sml-mode)))
           ("RUBY" (or
                    (mode . ruby-mode)
                    (mode . inf-ruby-mode)))
           ("SQL"  (or
                    (mode . sql-mode)
                    (mode . sql-interactive-mode)))
           ;; Email
           ("EMAIL" (or
                     (name . "^\\*mu4e")
                     (mode . mu4e:compose)))
           ;; Shells
           ("SHELL"  (or
                      (mode . sh-mode)
                      (mode . shell-mode)
                      (mode . ssh-mode)
                      (mode . eshell-mode)
                      (mode . term-mode)))
           ;; PDF
           ("PDF" (mode . pdf-view-mode))
           ;; Emacs related
           ("ELISP" (or
                     (mode . emacs-lisp-mode)
                     (mode . list-mode)
                     (mode . inferior-emacs-lisp-mode)))
           ("EMACS" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*Packages\\*$")))
           ;; Version control
           ("MAGIT"  (or
                      (mode . magit-mode)
                      (name . "^\\*magit")))
           ;; Services
           ("PRODIGY"  (or
                        (mode . prodigy-mode)
                        (name . "^\\*prodigy")))
           ;; All others
           ("OTHERS" (name . ".*")))))
  ;;
  ;; Set this buffer’s filter groups to saved version with NAME.
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

;;;
;;; Kill process buffer without confirmation?
;; https://emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
(setq kill-buffer-query-functions
      ;; Delete members of LIST which are eq to ELT, and return the result.
      (delq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
