;;; 200_minibuffer-related.el ---                    -*- lexical-binding: t; -*-

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


;;; Show last several messages in the mini-buffer.
;; http://mbork.pl/2017-05-01_show-some-last-messages
(defcustom default-messages-to-show 10
  "Default number of messages for `show-last-several-messages'.")
;;
(defun show-last-several-messages (count)
  "Show COUNT last lines of the `*Messages*' buffer."
  (interactive "P")
  (setq count (if count (prefix-numeric-value count)
		default-messages-to-show))
  (save-excursion
    (set-buffer "*Messages*")
    (let ((prev-point-max (point-max-marker))
	  (inhibit-read-only t))
      (message "%s"
               (progn
		 (set-buffer "*Messages*")
		 (buffer-substring-no-properties
		  (progn
                    (goto-char (point-max))
                    (unless (bolp)
                      (insert "\n"))
                    (forward-line (- count))
                    (point))
		  (point-max))))
      (delete-region (point-max) prev-point-max))))
