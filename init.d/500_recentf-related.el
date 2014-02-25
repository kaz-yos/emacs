;;; Recent files extended
;; Book by rubikitch p87
;; http://d.hatena.ne.jp/rubikitch/20091224/recentf
;; http://www.mygooglest.com/fni/dot-emacs.html
(setq recentf-save-file  "~/.emacs.d/recentf")		; save file within ~/.emacs.d
(setq recentf-max-saved-items 3000)
(require 'recentf-ext)
(global-set-key (kbd "C-S-z") 'recentf-open-files)	; helm has priority now
;; (global-set-key (kbd "C-z") 'helm-recentf)		; helm version. activated at helm configuration
;;
;; recentf auto-save
;; http://d.hatena.ne.jp/tomoya/20110217/1297928222
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '("recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))
;;
;; ;; Supress annoying message in mini-buffer	; 2013-09-27 Crashes by variable depth problems??
;; ;; http://masutaka.net/chalow/2011-10-30-2.html
;; ;; https://gist.github.com/1325654/955277113028eb7b968453a5b7802b74b51b393d
;; (defvar my-recentf-list-prev nil)
;; ;;
;; (defun my-recentf-save-list ()
;;   "If recentf-list and previous recentf-list is equal,
;; do nothing"
;;   (unless (equal recentf-list my-recentf-list-prev)
;;     (recentf-save-list)
;;     (setq my-recentf-list-prev recentf-list)))
;; ;;
;; (defadvice write-region
;;   (around recentf-no-message)
;;   (ad-set-arg 4 'nomsg)
;;   ad-do-it
;;   (set-buffer-modified-p nil))
;; ;;
;; (defadvice recentf-save-list
;;   (around no-message activate)
;;   "suppress the output from message() and write-region() to
;; minibuffer"
;;   (let ((activated (ad-is-active 'write-region)))
;;     (ad-enable-advice 'write-region 'around 'recentf-no-message)
;;     (ad-activate 'write-region)
;;     (unwind-protect
;; 	(flet ((message (format-string &rest args)
;; 			(eval `(format ,format-string ,@args))))
;; 	  ad-do-it)
;;       (ad-disable-advice 'write-region 'around 'recentf-no-message)
;;       (if activated
;; 	  (ad-activate 'write-region)
;; 	(ad-deactivate 'write-region)))))
;; ;;
;; (defadvice recentf-cleanup
;;   (around no-message activate)
;;   "suppress the output from message() to minibuffer"
;;   (flet ((message (format-string &rest args)
;; 		  (eval `(format ,format-string ,@args))))
;;     ad-do-it))
;; ;;
;; ;; (setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
;; ;; (setq recentf-max-saved-items 2000)
;; ;; (setq recentf-exclude '(".recentf"))
;; ;; (setq recentf-auto-cleanup 10)
;; ;; (run-with-idle-timer 30 t 'my-recentf-save-list)
;; ;; (recentf-mode 1)
