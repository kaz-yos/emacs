;;; Markdown mode
;; http://jblevins.org/projects/markdown-mode/
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


;;; Graphviz mode
;; http://ppareit.github.io/graphviz-dot-mode/
(require 'graphviz-dot-mode)
;; Font locking is automatic, indentation uses the same commands as
;; other modes, tab, M-j and C-M-q.  Insertion of comments uses the
;; same commands as other modes, M-; .  You can compile a file using
;; M-x compile or C-c c, after that M-x next-error will also work.
;; There is support for viewing an generated image with C-c p.
