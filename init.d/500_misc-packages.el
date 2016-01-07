;;;
;;; minor-mode-hack.el
;; M-x show-minor-mode-map-priority to see minor mode priority
(use-package minor-mode-hack
  :commands (show-minor-mode-map-priority))

;;;
;;; show-keys.el	; Show keys in a buffer as keys are typed.
;; http://www.youtube.com/watch?v=0cZ7szFuz18
;; https://github.com/AndreaCrotti/minimal-emacs-configuration
(use-package show-keys
  :commands (show-keys))

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
