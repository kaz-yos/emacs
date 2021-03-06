;;;
;;; manage-minor-mode.el
;; http://fukuyama.co/manage-minor-mode
(use-package manage-minor-mode
  :ensure t
  :commands (manage-minor-mode))


;;;
;;; command-log-mode.el
;; https://github.com/lewang/command-log-mode
(use-package command-log-mode
  :ensure t
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
;;; ESUP - Emacs Start Up Profiler
;; https://github.com/jschaf/esup
;; https://blog.d46.us/advanced-emacs-startup/
(use-package esup
  :ensure t
  :commands (esup))


;;;
;;; pubmed.el
;; https://gitlab.com/fvdbeek/emacs-pubmed
(use-package pubmed
  :ensure t
  :commands (pubmed-search
             pubmed-advanced-search))
