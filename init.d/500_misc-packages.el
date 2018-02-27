;;;
;;; manage-minor-mode.el
;; http://fukuyama.co/manage-minor-mode
(use-package manage-minor-mode
  :commands (manage-minor-mode))


;;;
;;; csv-mode.el
;; http://emacswiki.org/emacs/CsvMode
(use-package csv-mode
  :mode ("\\.csv" . csv-mod))


;;;
;;; demo-it.el
;; https://github.com/howardabrams/demo-it
(use-package demo-it
  :commands (demo-it-mode
             demo-it-start))


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


;;;
;;;
;; https://github.com/Malabarba/paradox
(use-package paradox
  :commands (paradox-enable)
  :init
  (add-hook 'after-init-hook 'paradox-enable))
