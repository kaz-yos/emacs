;;; init_org_async.el --- Minimum init file for async org-mode      -*- lexical-binding: t; -*-

;; Specify this file in `org-export-async-init-file'
;; Errors in async process can be examined using M-x org-export-stack

;;; 
;;; Configure directories
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; 
;;; Load org-mode and exporter
(require 'org)
(require 'ox)
