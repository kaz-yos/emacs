;;; view-mode			; C-x C-r to open in view-mode. Requires viewer.el
;; http://www.emacswiki.org/emacs/ViewMode
(global-set-key (kbd "A-o") 'read-only-mode)


;;; view-read-only
;; view-read-only is a variable defined in `files.el'.
;; Non-nil means buffers visiting files read-only do so in view mode.
(setq view-read-only t)


;;; viewer.el
(use-package viewer
  :ensure t
  :commands (viewer-change-modeline-color)
  :init
  (add-hook 'view-mode-hook 'viewer-change-modeline-color)
  :config
  ;; Setup stay-in view-mode.
  (viewer-stay-in-setup)
  ;; Change mode-line color
  (setq viewer-modeline-color-unwritable "tomato")
  (setq viewer-modeline-color-view "#9d5700")
  ;; Set up
  (viewer-change-modeline-color-setup))


;;; view.el
(use-package view
  :commands (view-mode)
  :config
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
  (define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "K") 'View-scroll-line-backward)
  (define-key view-mode-map (kbd "i") #'(lambda ()
                                          (interactive)
                                          (read-only-mode -1)))
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
  ;; Do not exit view-mode if non-writable
  ;; http://www.emacswiki.org/emacs-es/ViewMode#toc4
  (defvar view-mode-force-exit nil)
  (defmacro do-not-exit-view-mode-unless-writable-advice (f)
    `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
       (if (and (buffer-file-name)
                (not view-mode-force-exit)
                (not (file-writable-p (buffer-file-name))))
           (message "File is unwritable, so stay in view-mode.")
         ad-do-it)))
  ;; Add advice to disable view-mode-exit and view-mode-disable
  (do-not-exit-view-mode-unless-writable-advice view-mode-exit)
  (do-not-exit-view-mode-unless-writable-advice view-mode-disable)
  ;; Also disable toggle-read-only, which is most often used
  (do-not-exit-view-mode-unless-writable-advice toggle-read-only))
