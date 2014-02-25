;;; evil
;; http://www.emacswiki.org/emacs/Evil
;; Activate evil (2014-02-03 conflict with helm C-z)
(require 'evil)
;; (evil-mode 1)	; not by default

;;; Making the most of RET and SPC
;; Keep RET and SPC bindings.
;; http://www.emacswiki.org/emacs/Evil
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")










