;;; mini-buffer configuration 2014-02-03
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; Larger font size
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((default :height 1.2))))	; Larger font size

















