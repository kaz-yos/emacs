;;; Auto-saving buffers
;; auto-save-buffers: Never lose your data
;; http://0xcc.net/misc/auto-save/
;; http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 1 t 'auto-save-buffers)	; auto-save if idle for 1 sec
;;
;; auto-save-buffers-enhanced.el
;; http://qiita.com/ongaeshi/items/8cbd8d3c792476c59a11
;; http://blog.sanojimaru.com/post/20090254216/emacs
(require 'auto-save-buffers-enhanced)
;; Only in Git, CVS, or Subversion directories
;; (auto-save-buffers-enhanced-include-only-checkout-path t)
;; Timing of save
;; (setq auto-save-buffers-enhanced-interval 1)	; 0.5 sec by default
;; Quiet save
(setq auto-save-buffers-enhanced-quiet-save-p t)
;; Activate
(auto-save-buffers-enhanced t)
;; (setq auto-save-buffers-enhanced-include-regexps '(".+"))
;; (setq auto-save-buffers-enhanced-exclude-regexps '("^not-save-file" "\\.ignore$"))



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
