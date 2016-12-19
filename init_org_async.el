;;; init_org_async.el --- Minimum init file for async org-mode      -*- lexical-binding: t; -*-

;; Specify this file in `org-export-async-init-file'
;; Errors in async process can be examined using M-x org-export-stack

;;;
;;; Configure directories
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Loading
;; Some packages use defstruct, which is obsolete. Alias cl-defstruct to avoid errors.
;; 2016-01-09 latex-math-preview still depends on defstruct, and gives an error w/o it.
(defalias 'defstruct 'cl-defstruct)
;;
;; Need to be initialized.
(package-initialize)


;;;
;;; use-package.el
(require 'use-package)

;;;
;;; Configure org-mode
(require 'org)
;;; Load plugins
(require 'org-ref)
;;; Load exporters
(require 'ox)
(require 'ox-beamer)
(require 'ob-R)
(require 'ob-python)
(require 'ob-ditaa)
(require 'ob-dot)
;;; Language packages
;;; ESS
(require 'ess-site)
(setq inferior-R-args "--no-restore-history --no-save ")
(setq ess-ask-for-ess-directory nil)
;;
;;; Enable languages
;; Org-Mode Evaluation of code disabled
;; https://github.com/syl20bnr/spacemacs/issues/7641
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
   (latex . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-R.html
   (R . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-python.html
   (python . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-ditaa.html
   (ditaa . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
   (dot . t)
   ;;
   (emacs-lisp . t)
   (shell . t)))
;;
;; Do not ask code execution confirmation
(setq org-confirm-babel-evaluate nil)
;;
;; Language specific settings
;; ditaa diagram language (git repo org-mode has necessary scripts)
(setq org-ditaa-jar-path "~/.emacs.d/packages/org-mode/contrib/scripts/ditaa.jar")
;;
;;
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
;;
;; In case there are missed configurations. (loading this way may not be functions)
;; (load "~/.emacs.d/init.d/500_org-mode-related.el")
