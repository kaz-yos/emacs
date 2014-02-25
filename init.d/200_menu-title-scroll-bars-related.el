;;; Bars: Menu bar only. No scroll bar or tool bar.
;; http://www.emacswiki.org/emacs/FullScreen#toc7
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show the full path in the frame bar (title bar)
;; http://stackoverflow.com/questions/8945056/emacs-how-to-show-the-current-directory-in-the-frame-bar
(setq frame-title-format '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))

