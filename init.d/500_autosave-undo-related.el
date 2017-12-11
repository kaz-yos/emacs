;;; 500_autosave-undo-related.el ---                 -*- lexical-binding: t; -*-

;;;
;;; super-save.el
;; http://emacsredux.com/blog/2016/01/30/super-save/
;; https://github.com/bbatsov/super-save
(use-package super-save
  :diminish super-save-mode
  :config
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
  (defun quiet-super-save-command (orig-fun)
    (with-suppressed-message (funcall orig-fun)))
  (advice-add 'super-save-command
              :around
              #'quiet-super-save-command)
  ;; Only when idle for some time
  ;; This is active in ssh buffers, too
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 5)
  ;; Activate
  (super-save-mode +1))


;;;
;;; Undo tree for undoing edits visually
;; http://www.emacswiki.org/emacs/UndoTree
;; C-/ for undo. C-? (C-S-/) for redo.
(use-package undo-tree
  :config
  (setq undo-tree-mode-lighter "")
  ;; Active everywhere
  (global-undo-tree-mode))
