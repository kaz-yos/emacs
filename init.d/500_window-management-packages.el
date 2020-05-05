;;; 500_window-management-packages.el ---            -*- lexical-binding: t; -*-


;;;
;;; windresize.el
;; M-x windresize, arrows, C-g for cancel, RET to save
(use-package windresize
  :ensure t
  :bind-keymap (("C-c m w" . windresize-map))
  :commands windresize)


;;;
;;; window-number.el for direct movement to windows
(use-package window-number
  :ensure t
  :config
  ;; No need to show numbers in the mode-line.
  ;; (window-number-mode 1)
  ;;
  (defun my-window-number-select (number)
    "Selects the nth window."
    (interactive "P")
    (if (integerp number)
        (let ((window (nth (1- number) (window-number-list))))
          (if (and window (or (not (window-minibuffer-p window))
                              (minibuffer-window-active-p window)))
              (select-window window)
            ;; if not found, activate ace-window
            (ace-select-window)))))
  ;;
  (global-set-key (kbd "s-1") (lambda () (interactive) (my-window-number-select 1)))
  (global-set-key (kbd "s-2") (lambda () (interactive) (my-window-number-select 2)))
  (global-set-key (kbd "s-3") (lambda () (interactive) (my-window-number-select 3)))
  (global-set-key (kbd "s-4") (lambda () (interactive) (my-window-number-select 4))))
