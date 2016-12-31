;;;
;;; minor-mode-hack.el
;; M-x show-minor-mode-map-priority to see minor mode priority
(use-package minor-mode-hack
  :commands (show-minor-mode-map-priority))


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
