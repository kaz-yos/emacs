;;; init_org_async.el --- Minimum init file for async org-mode      -*- lexical-binding: t; -*-

;; Specify this file in `org-export-async-init-file'
;; Errors in async process can be examined using M-x org-export-stack

;;; Configure directories
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Initialize packages so that they can be auto-loaded.
(defalias 'defstruct 'cl-defstruct)
(package-initialize t)


;;;
;;; No emacs-default autosave or backup
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html
(setq auto-save-default nil)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html
(setq make-backup-files nil)


;;;
;;; Configure org-mode
(add-to-list 'load-path
             ;; We need a wild card as a MELPA package keeps changing the folder name.
             ;; http://emacs.stackexchange.com/questions/9768/elisp-files-in-load-path-are-not-loaded-on-emacs-start
             ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html
             (car (last (file-expand-wildcards "~/.emacs.d/elpa/org-plus-contrib*"))))
(require 'org)
(require 'org-ref)
;;; Load exporters
(require 'ox-beamer)
;;
;;
;;; Enable languages
;; Org-Mode Evaluation of code disabled
;; https://github.com/syl20bnr/spacemacs/issues/7641
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)
   (R . t)
   (python . t)
   (ditaa . t)
   (dot . t)
   ;; This does not work unless there is a slime buffer.
   ;; (lisp . t)
   (emacs-lisp . t)
   (shell . t)))
;;
;; Do not ask code execution confirmation
(setq org-confirm-babel-evaluate nil)
;;
;; Org-mode: Source block doesn't respect parent buffer indentation
;; http://emacs.stackexchange.com/questions/9472/org-mode-source-block-doesnt-respect-parent-buffer-indentation
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)
;;
;;
;;; Language specific settings
;;
;;;  latex
;; PDF processing with correct bibtex handling
;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-05/msg00791.html
;; %f: full file name
;; %b: file base name
;; %o: base directory
(setq org-latex-pdf-process
      '("latexmk -f %f"))
;; Remove additional temporary files.
(setq org-latex-logfiles-extensions
      (append '("dvi" "bbl") org-latex-logfiles-extensions))
;; Support jsarticle
(add-to-list 'org-latex-classes
             '("jsarticle"
               "\\documentclass[a4j]{jsarticle}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; code highlight
(setq org-latex-listings 'minted)
;;
;;;  R
(setq inferior-R-args "--no-restore-history --no-save ")
(setq ess-ask-for-ess-directory nil)
;;
;;;  slime
;; This does not work unless there is a slime process already running (not possible in async).
;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-07/msg01237.html
;; (setq org-babel-lisp-eval-fn #'sly-eval)
(setq inferior-lisp-program (executable-find "sbcl"))
;;
;;;  ditaa diagram language
;; (git repo org-mode has necessary scripts)
(setq org-ditaa-jar-path "~/.emacs.d/packages/org-mode/contrib/scripts/ditaa.jar")
