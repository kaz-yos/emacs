;;; No auto filling in text mode
;; http://tomikura.s2.xrea.com/linux/install/emacs.html
(setq fill-column 80)
(setq text-mode-hook '(lambda () (auto-fill-mode 0)))
;; opposite setting
; wrap long lines in text mode
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;
;;; Default major mode set as text-mode
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html
(setq-default major-mode 'text-mode)
;;
;;; Manual filling
(global-set-key (kbd "C-c f") 'fill-paragraph)
