;;; init_org_async.el --- Minimum init file for async org-mode      -*- lexical-binding: t; -*-

;; Specify this file in `org-export-async-init-file'
;; Errors in async process can be examined using M-x org-export-stack

;;; Configure directories
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))


;;;
;;; Configure org-mode
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
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))
;;
;;;  R
(setq inferior-R-args "--no-restore-history --no-save ")
(setq ess-ask-for-ess-directory nil)
;;
;;;  slime
;; This does not work unless there is a slime process already running (not possible in async).
;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-07/msg01237.html
(setq inferior-lisp-program (executable-find "sbcl"))
;;
;;;  ditaa diagram language
;; (git repo org-mode has necessary scripts)
(setq org-ditaa-jar-path "~/.emacs.d/packages/org-mode/contrib/scripts/ditaa.jar")
