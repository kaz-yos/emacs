;;; recentf.el
;; https://www.emacswiki.org/emacs/RecentFiles
(use-package recentf
  :config
  (setq recentf-save-file  "~/.emacs.d/recentf")
  (setq recentf-max-saved-items 3000)
  (setq recentf-exclude '("recentf$"
                          ".*\\.maildir.*"
                          "/var/folders/.*"
                          ".*company-statistics.*"))
  ;; Define when to automatically cleanup the recent list.
  ;; - `mode'
  ;; Cleanup when turning the mode on (default).
  ;; - `never'
  ;; Never cleanup the list automatically.
  ;; - A number
  ;; Cleanup each time Emacs has been idle that number of seconds.
  ;; - A time string
  ;; Cleanup at specified time string, for example at "11:00pm".
  (setq recentf-auto-cleanup 'mode)
  ;; Auto save when idle
  ;; (run-with-idle-timer SECS REPEAT FUNCTION &rest ARGS)
  (run-with-idle-timer 15 t '(lambda ()
                               (with-suppressed-message (recentf-save-list)))))


;;; recentf-ext.el
;; http://d.hatena.ne.jp/rubikitch/20091224/recentf
;; https://www.emacswiki.org/emacs/RecentFiles#toc16
;; Lisp:recentf-ext.el extends recentf package.
;; ‘dired’ buffers can be handled.
;; Switching to file buffer considers it as most recent file.
(use-package recentf-ext
  :config
  ;; Activate recentf-mode
  (recentf-mode 1))
