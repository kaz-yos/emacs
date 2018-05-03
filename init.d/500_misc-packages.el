;;;
;;; manage-minor-mode.el
;; http://fukuyama.co/manage-minor-mode
(use-package manage-minor-mode
  :commands (manage-minor-mode))


;;;
;;; command-log-mode.el
;; https://github.com/lewang/command-log-mode
(use-package command-log-mode
  :commands (command-log-mode
             global-command-log-mode)
  :config
  ;; No enlarging
  (setq command-log-mode-window-font-size 1)
  ;; Does opening the command log turn on the mode?
  (setq command-log-mode-open-log-turns-on-mode t)
  ;; Global activation
  (setq command-log-mode-is-global t)
  ;; Show the command-log window or frame automatically.
  (setq command-log-mode-auto-show t))
