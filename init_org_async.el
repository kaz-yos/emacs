;;; init_org_async.el --- Minimum init file for async org-mode      -*- lexical-binding: t; -*-

;; Specify this file in `org-export-async-init-file'
;; Errors in async process can be examined using M-x org-export-stack

;;;
;;; Configure directories
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

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
;;
;;; Enable languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
   (latex . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-R.html
   (R . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-stan.html
   (stan . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
   (clojure . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-python.html
   (python . t)
   ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-ditaa.html
   (ditaa . t)
   (emacs-lisp . t)
   (shell . t)))
;;
;; Language specific settings
;; ditaa diagram language (git repo org-mode has necessary scripts)
(setq org-ditaa-jar-path "~/.emacs.d/packages/org-mode/contrib/scripts/ditaa.jar")
;;
;;
;; In case there are missed configurations. (loading this way may not be functions)
(load "~/.emacs.d/init.d/500_org-mode-related.el")
