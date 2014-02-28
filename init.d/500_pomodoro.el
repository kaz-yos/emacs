;;; pomodoro.el by syohex
;; http://d.hatena.ne.jp/syohex/20121215/1355579575
;; M-x auto-install-from-url https://raw.github.com/syohex/emacs-utils/master/pomodoro.el
(require 'pomodoro)

;; Open this file at the endo of pomodoro
(setq pomodoro:file "~/.emacs.d/pomodoro.org")

;; time settings
(setq pomodoro:work-time 25
      pomodoro:rest-time 5
      pomodoro:long-rest-time 30)

;; Notification
;; (require 'notifications) ;; Requires Linux and DBUS
;; (defun* my/pomodoro-notification (&key (title "Pomodoro")
;;                                        body
;;                                        (urgency 'critical))
;;   (notifications-notify :title title :body body :urgency urgency))

;; After pomodoro
(add-hook 'pomodoro:finish-work-hook
          (lambda ()
            (message "Pomodoro finished")))

;; After rest
(add-hook 'pomodoro:finish-rest-hook
          (lambda ()
            (message "Break over")))

;; After long break
(add-hook 'pomodoro:long-rest-hook
          (lambda ()
            (message "Long break over")))















