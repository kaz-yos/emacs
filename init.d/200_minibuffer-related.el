;;; mini-buffer configuration


;;; Larger font size
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.2))))
;;
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)


;;; Improve coding experience within mini-buffer
;; http://emacsredux.com/blog/2016/03/02/pimp-my-minibuffer/
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
