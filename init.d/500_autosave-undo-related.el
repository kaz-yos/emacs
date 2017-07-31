;;; Auto-saving buffers

;;;
;;; super-save.el
;; http://emacsredux.com/blog/2016/01/30/super-save/
;; https://github.com/bbatsov/super-save
(use-package super-save
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
  ;; Mute the mode-line 2014-02-02
  (setq undo-tree-mode-lighter "")
  ;; Active everywhere
  (global-undo-tree-mode))


;;;
;;; Cursor move undo
;;
;;; point-undo.el
;; http://d.hatena.ne.jp/rubikitch/20081230/pointundo
;; (require 'point-undo)
(define-key global-map (kbd "s-p") 'point-undo)
(define-key global-map (kbd "s-n") 'point-redo)
;;
;;; goto-chg.el
;; http://www.emacswiki.org/emacs/GotoChg
(require 'goto-chg)
(global-set-key (kbd "A-M-p") 'goto-last-change)
(global-set-key (kbd "A-M-n") 'goto-last-change-reverse)
;;
;;
;; ;;; Extended point-undo.el
;; ;; http://qiita.com/zk_phi/items/c145b7bd8077b8a0f537
;; (require 'ring)
;; (require 'edmacro)
;; ;;
;; (defvar-local jump-back!--marker-ring nil)
;; ;;
;; (defun jump-back!--ring-update ()
;;   (let ((marker (point-marker)))
;;     (unless jump-back!--marker-ring
;;       (setq jump-back!--marker-ring (make-ring 30)))
;;     (ring-insert jump-back!--marker-ring marker)))
;; ;;
;; (run-with-idle-timer 1 t 'jump-back!--ring-update)
;; ;;
;; (defun jump-back! ()
;;   (interactive)
;;   (if (ring-empty-p jump-back!--marker-ring)
;;       (error "No further undo information")
;;     (let ((marker (ring-ref jump-back!--marker-ring 0))
;;           (repeat-key (vector last-input-event)))
;;       (ring-remove jump-back!--marker-ring 0)
;;       (if (= (point-marker) marker)
;;           (jump-back!)
;;         (goto-char marker)
;;         (message "(Type %s to repeat)" (edmacro-format-keys repeat-key))
;;         (set-temporary-overlay-map
;;          (let ((km (make-sparse-keymap)))
;;            (define-key km repeat-key 'jump-back!)
;;            km))))))
;; ;;
;; (global-set-key (kbd "s-p") 'jump-back!)
