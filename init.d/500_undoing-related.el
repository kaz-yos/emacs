;;; Undo tree for undoing edits visually
;; http://www.emacswiki.org/emacs/UndoTree
;; C-/ for undo. C-? (C-S-/) for redo.
(require 'undo-tree)
;; Mute the mode-line 2014-02-02
(setq undo-tree-mode-lighter "")
;; Active everywhere
(global-undo-tree-mode)

;;; Cursor move undo
;;
;; point-undo.el
;; http://d.hatena.ne.jp/rubikitch/20081230/pointundo
(require 'point-undo)
;; (define-key global-map (kbd "<f5>") 'point-undo)
;; (define-key global-map (kbd "S-<f5>") 'point-redo)
(define-key global-map (kbd "<C-f5>")   'point-undo)
(define-key global-map (kbd "C-S-<f5>") 'point-redo)
;;
;; goto-chg.el
;; http://www.emacswiki.org/emacs/GotoChg
(require 'goto-chg)
;; (global-set-key (kbd "<C-f5>") 'goto-last-change)
;; (global-set-key (kbd "C-S-<f5>") 'goto-last-change-reverse)
(global-set-key (kbd "<f5>")   'goto-last-change)
(global-set-key (kbd "S-<f5>") 'goto-last-change-reverse)
