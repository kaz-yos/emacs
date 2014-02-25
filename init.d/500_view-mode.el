;;; view-mode			; C-x C-r to open in view-mode. Requires viewer.el
;; Book by rubikitch p214-
;; http://d.hatena.ne.jp/rubikitch/20081104/1225745862
;; http://d.hatena.ne.jp/syohex/20110114/1294958917
(setq view-read-only t)	; buffers visiting files read-only do so in view mode
(require 'viewer)
(viewer-stay-in-setup)
(setq viewer-modeline-color-unwritable "tomato"
      viewer-modeline-color-view "salmon")	; color changed from orange 2014-02-02
(viewer-change-modeline-color-setup)
(require 'view)
;; less like
(define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
(define-key view-mode-map (kbd "?") 'View-search-regexp-backward )
(define-key view-mode-map (kbd "G") 'View-goto-line-last)
(define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
(define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
;; vi/w3m like
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "j") 'next-line)
(define-key view-mode-map (kbd "k") 'previous-line)
(define-key view-mode-map (kbd "l") 'forward-char)
(define-key view-mode-map (kbd "J") 'View-scroll-line-forward)	; no cursor move, screen one line up
(define-key view-mode-map (kbd "K") 'View-scroll-line-backward)	; no cursor move, screen one line down
;; (define-key view-mode-map (kbd "i") 'read-only-mode)		; Get out of read-only-mode
;; Space bar use
(define-key view-mode-map (kbd " ") 'scroll-up)
(define-key view-mode-map (kbd "S-SPC") 'scroll-down)
;; for bm.el
;; (define-key view-mode-map (kbd ".") 'bm-toggle)
;; (define-key view-mode-map (kbd "[") 'bm-previous)
;; (define-key view-mode-map (kbd "]") 'bm-next)
(define-key view-mode-map (kbd "[") 'my-bm-previous)
(define-key view-mode-map (kbd "]") 'my-bm-next)
;; for highlight-symbol.el
;; (define-key view-mode-map (kbd ".") 'highlight-symbol-at-point)
;; (define-key view-mode-map (kbd "[") 'my-highlight-symbol-prev)
;; (define-key view-mode-map (kbd "]") 'my-highlight-symbol-next)
;;
;; Open non-writable files in view-mode	; (setq view-read-only t) sufficient? 2014-02-03
;; (defadvice find-file
;;   (around find-file-switch-to-view-file (file &optional wild) activate)
;;   (if (and (not (file-writable-p file))
;;            (not (file-directory-p file)))
;;       (view-file file)
;;     ad-do-it))
;; Do not exit view-mode if non-writable
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))
;; Add advice
(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)
;;
;; Key-bind used instead of "jk"
(global-set-key (kbd "C-c l") 'read-only-mode)
