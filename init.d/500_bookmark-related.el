;;; bm.el	Within-file bookmarking
;; See ~/.emacs.d/elpa/bm-readme.txt
;; http://d.hatena.ne.jp/peccu/20100402
;; Saving bookmarks
(setq-default bm-buffer-persistence t)
(setq bm-repository-file "~/.emacs.d/bm-el-repository")
(setq bm-restore-repository-on-load t)	; bm-readme.txt
;;
;; No annotation column because I do not used it
(setq bm-show-annotations t)   ; Do show for spacing
(setq bm-annotation-width 5)   ; Minimum width
(setq bm-header-annotation "") ; No need to show "Annotation"
;;
;; Load
(require 'bm)
;; Load on startup
(add-hook 'after-init-hook		'bm-repository-load)
;; Restore when finding file
(add-hook 'find-file-hooks		'bm-buffer-restore)
;;
;; Saving on killing and saving a buffer
(add-hook 'kill-buffer-hook		'bm-buffer-save)
(add-hook 'auto-save-hook		'bm-buffer-save)
(add-hook 'after-save-hook		'bm-buffer-save)
;; Version control (Rubikitch book p116)
(add-hook 'after-revert-hook		'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook	'bm-buffer-save)
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))
;;
;; Define function to do bm-previous/next and recenter
(defun my-bm-next ()
  (interactive)
  (bm-next)
  (recenter "Top"))
(defun my-bm-previous ()
  (interactive)
  (bm-previous)
  (recenter "Top"))
;;
;; Keyboard
(global-set-key (kbd "M-SPC")	'bm-toggle)	; Conflict with IM. Use ESC-SPC, which is the same
;; (global-set-key (kbd "M-]")	'bm-next)
;; (global-set-key (kbd "M-[")	'bm-previous)
(global-set-key (kbd "M-]")	'my-bm-next)
(global-set-key (kbd "M-[")	'my-bm-previous)
(global-set-key (kbd "C-c b")	'bm-show)
;;
;; helm-bm.el		; helm sources for bm.el
(require 'helm-bm) ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-c b") 'helm-bm)




;;; Built-in bookmarks (not used)
;; http://www.emacswiki.org/emacs/BookMarks#toc6
(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs.d/bookmarks")		; save file within ~/.emacs.d
(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))
;;
;; Key-bindings (not configured by default in 24.2) ; Does not work
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html
;; (global-set-key (kbd "C-x r m") 'bookmark-set)
;; (global-set-key (kbd "C-x r b") 'bookmark-jump)
;; (global-set-key (kbd "C-x r l") 'list-bookmarks)
