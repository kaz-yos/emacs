;;;
;;; MARKDOWN-RELATED
;;;  markdown-mode.el
;; http://jblevins.org/projects/markdown-mode/
;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :commands (open-md-in-atom
             open-md-in-markoff)
  :mode ("\\.md" . markdown-mode)
  ;;
  :config
  ;; https://github.com/defunkt/markdown-mode#customization
  (setq markdown-fontify-code-blocks-natively t)
  ;;
  ;; http://qiita.com/takuma510/items/77489cf538580dfcc41d
  (defun open-md-in-atom ()
    "Open current file in Atom for real-time preview"
    (interactive)
    (call-process
     "atom" nil nil nil buffer-file-name))
  ;;
  (defun open-md-in-markoff ()
    "Open current markdown file "
    (interactive)
    ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html
    (shell-command (concat "open -a /Applications/Markoff.app"
                           " "
                           buffer-file-name))))


;;;  markdown-preview-mode.el
;; https://github.com/ancane/markdown-preview-mode
(use-package markdown-preview-mode
  :commands (markdown-preview-mode
             markdown-preview-open-browser))


;;;
;;; graphviz-dot-mode.el
;; http://ppareit.github.io/graphviz-dot-mode/
(use-package graphviz-dot-mode
  :mode ("\\.dot" . graphviz-dot-mode))
;; Font locking is automatic, indentation uses the same commands as
;; other modes, tab, M-j and C-M-q.  Insertion of comments uses the
;; same commands as other modes, M-; .  You can compile a file using
;; M-x compile or C-c c, after that M-x next-error will also work.
;; There is support for viewing an generated image with C-c p.


;;;
;;; tagedit.el
;; A collection of paredit-like functions for editing in html-mode.
;; https://github.com/magnars/tagedit
(use-package tagedit
  :after sgml-mode
  :hook ((html . (lambda () (tagedit-mode 1))))
  :config
  (tagedit-add-paredit-like-keybindings))
